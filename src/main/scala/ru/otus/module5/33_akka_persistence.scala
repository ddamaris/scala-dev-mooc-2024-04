package ru.otus.module5

import akka.{Done, NotUsed, projection}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Props, SpawnProtocol}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.persistence.cassandra.query.scaladsl.CassandraReadJournal
import akka.persistence.query.{EventEnvelope, Offset, PersistenceQuery, typed}
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.EventSourcedBehavior
import akka.persistence.typed.scaladsl.Effect
import akka.stream.scaladsl.{Sink, Source}
import ru.otus.module5.Product.{AddProduct, ChangeTitle, Command, Event, RemoveProduct}
import ru.otus.module5.simpleActors.Supervisor
import akka.actor.typed.scaladsl.AskPattern._
import akka.cluster.sharding.typed.ShardingEnvelope
import akka.cluster.sharding.typed.scaladsl.{ClusterSharding, Entity, EntityRef, EntityTypeKey, ShardedDaemonProcess}
import akka.cluster.typed.{ClusterSingleton, SingletonActor}
import akka.projection.{ProjectionBehavior, ProjectionId, eventsourced}
import akka.projection.eventsourced.scaladsl.EventSourcedProvider
import akka.projection.scaladsl.{AtLeastOnceProjection, SourceProvider}
import akka.projection.slick.{SlickHandler, SlickProjection}
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.{Logger, LoggerFactory}
import ru.otus.module5
import slick.basic.DatabaseConfig
import slick.dbio.{DBIO, Effect => dbEffect}
import slick.jdbc.PostgresProfile

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.concurrent.duration.DurationInt

case class ProductEntity(id: Int, title: String) extends CborSerializable with Serializable

trait CborSerializable

object ProductTags{
  val ProductAdded = "product_added"
  val TitleChanged = "product_changed"
  val ProductRemoved = "product_removed"
}

object Product{

  type Command = ProductCommand
  sealed trait ProductCommand extends CborSerializable
  case class AddProduct(id: Int, title: String) extends ProductCommand
  case class ChangeTitle(id: Int, newTitle: String) extends ProductCommand
  case class RemoveProduct(id: Int) extends ProductCommand

  type Event = ProductEvent
  sealed trait ProductEvent extends CborSerializable
  case class ProductAdded(id: Int, title: String) extends ProductEvent
  case class TitleChanged(id: Int, newTitle: String) extends ProductEvent
  case class ProductRemoved(id: Int) extends ProductEvent

  sealed trait State extends CborSerializable
  case object BlankState extends State

  case class ProductState private(product: Map[Int, String]) extends State{
    def addProduct(id: Int, title: String): State =
      this.copy(product.updated(id, title))
    def updateProduct(id: Int, newTitle: String): State =
      this.copy(product.updatedWith(id)(_ => Some(newTitle)))
    def removeProduct(id: Int): State = {
      val p = product.removed(id)
      if(p.isEmpty) BlankState else this.copy(p)
    }
  }

  object State{
    def empty: State = BlankState
    def apply(id: Int, title: String): State = ProductState(Map(id -> title))
  }

  val persistenceId = PersistenceId.ofUniqueId("Product")

  def apply(persistenceId: PersistenceId): Behavior[ProductCommand] =
    Behaviors.setup{ ctx =>
      ctx.log.info("Starting Product {}")
      EventSourcedBehavior[Command, Event, State](
        persistenceId,
        emptyState = State.empty,
        commandHandler,
        eventHandler
      ).withTagger {
        case ProductAdded(_, _) => Set(ProductTags.ProductAdded)
        case TitleChanged(_, _) => Set(ProductTags.TitleChanged)
        case ProductRemoved(_) => Set(ProductTags.ProductRemoved)
      }
    }

  private val commandHandler: (State, Command) => Effect[Event, State] = {(state, command) =>
    state match {
      case BlankState =>
        command match {
          case cmd: AddProduct => addProduct(cmd)
          case cmd: ChangeTitle => Effect.unhandled
          case cmd: RemoveProduct => Effect.unhandled
        }
      case _: ProductState =>
        command match {
          case cmd: AddProduct => addProduct(cmd)
          case cmd: ChangeTitle => changeTitle(cmd)
          case cmd: RemoveProduct => removeProduct(cmd)
        }
    }
  }

  private val eventHandler: (State, Event) => State = {(state, event) =>
    state match {
      case BlankState =>
        event match {
          case ProductAdded(id, title) =>
            State(id, title)
          case TitleChanged(_, _) =>
            throw new IllegalStateException(s"unexpected event [$event] in state [${state}]")
          case ProductRemoved(_) =>
            throw new IllegalStateException(s"unexpected event [$event] in state [${state}]")
        }
      case ps @ ProductState(_) =>
        event match {
          case ProductAdded(id, title) =>
            ps.addProduct(id, title)
          case TitleChanged(id, newTitle) =>
            ps.updateProduct(id, newTitle)
          case ProductRemoved(id) =>
            ps.removeProduct(id)
        }
    }
  }

  private def addProduct(cmd: AddProduct): Effect[Event, State] = {
    val evt = ProductAdded(cmd.id, cmd.title)
    Effect.persist(evt)
  }

  private def removeProduct(cmd: RemoveProduct): Effect[Event, State] = {
    val evt = ProductRemoved(cmd.id)
    Effect.persist(evt)
  }

  private def changeTitle(cmd: ChangeTitle): Effect[Event, State] = {
    val evt = TitleChanged(cmd.id, cmd.newTitle)
    Effect.persist(evt)
  }


}

class ReadProductJournalExample(system: ActorSystem[_]){
  val journal: CassandraReadJournal = PersistenceQuery(system)
    .readJournalFor[CassandraReadJournal](CassandraReadJournal.Identifier)
  val sourceByPersistenceId: Source[EventEnvelope, NotUsed] =
    journal.eventsByPersistenceId(Product.persistenceId.id, 0, Long.MaxValue)

  def sourceByTag(tag: String) =
    journal.eventsByTag(tag, Offset.noOffset)

  val sinkLog = Sink.foreach[EventEnvelope](ev => system.log.info(s"Retrieved event [${ev.event}]"))

  val stream = sourceByPersistenceId.to(sinkLog)

  def streamByTag(tag: String) =
    sourceByTag(tag).to(sinkLog)
}

class ProductRepository(val dbConfig: DatabaseConfig[PostgresProfile]){
  import dbConfig.profile.api._
  private class ProductTable(tag: Tag) extends Table[ProductEntity](tag, "product"){
    def id = column[Int]("id", O.PrimaryKey)
    def title = column[String]("title")

    def * = (id, title).mapTo[ProductEntity]
  }
  private val productTable = TableQuery[ProductTable]

  def getById(id: Int)(implicit ec: ExecutionContext): Future[Option[ProductEntity]] = {
    val q = productTable.filter(_.id === id)
    dbConfig.db.run(q.result.headOption)
  }

  def save(product: ProductEntity)(implicit ec: ExecutionContext) =
    productTable.insertOrUpdate(product).map(_ => Done)

  def delete(id: Int)(implicit ec: ExecutionContext) =
    productTable.filter(_.id === id).delete.map(_ => Done)
}

class ProductProjection(dbConfig: DatabaseConfig[PostgresProfile],
                        repository: ProductRepository, logger: Logger)(implicit system: ActorSystem[_]){
  val sourceProvider: SourceProvider[Offset, eventsourced.EventEnvelope[Event]] =
    EventSourcedProvider
    .eventsByTag[Product.Event](system, CassandraReadJournal.Identifier, ProductTags.ProductAdded)

  def projection(tag: String) = SlickProjection.exactlyOnce(
    projectionId = ProjectionId("product", tag),
    sourceProvider,
    dbConfig,
    handler
  )

  def handler(): SlickHandler[eventsourced.EventEnvelope[Event]] =
    new ProductHandler(repository, logger)(system.executionContext)
}

class ProductHandler(repository: ProductRepository, log: Logger)(implicit ec: ExecutionContext)
  extends SlickHandler[eventsourced.EventEnvelope[Event]]{

  override def process(envelope: eventsourced.EventEnvelope[Event]): DBIO[Done] = envelope.event match {
    case Product.ProductAdded(id, title) =>
      log.info(s"Product id [$id] was added")
      repository.save(ProductEntity(id, title))
    case Product.TitleChanged(id, newTitle) =>
      log.info(s"Product id [$id] title was changed")
      repository.save(ProductEntity(id, newTitle))
    case Product.ProductRemoved(id) =>
      log.info(s"Product id [$id] was deleted")
      repository.delete(id)
  }
}

object ProductApp{
  def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem[SpawnProtocol.Command] = ActorSystem(Supervisor(), "ProductApp")
    implicit val ec = system.executionContext
    implicit val timeout = Timeout(1 seconds)
    val logger: Logger = system.log
    val dbConfig: DatabaseConfig[PostgresProfile] =
      DatabaseConfig.forConfig("akka.projection.slick", system.settings.config)
    val productRepository = new ProductRepository(dbConfig)
    val productProjection = new ProductProjection(dbConfig, productRepository, logger)
    val ProductReadTypeKey = EntityTypeKey[ProjectionBehavior.Command]("Product")
    val tags = Vector.tabulate(5)(i => s"product-$i")
//    val sharding = ClusterSharding(system)
//    sharding.init(Entity(ProductReadTypeKey)(ctx => ProjectionBehavior(productProjection.projection(ctx.entityId))))
    ShardedDaemonProcess(system).init[ProjectionBehavior.Command](
      name = "product",
      numberOfInstances = tags.size,
      behaviorFactory = (i: Int) => ProjectionBehavior(productProjection.projection(tags(i))),
      stopMessage = ProjectionBehavior.Stop
    )

    Thread.sleep(5000)
    val singleton = ClusterSingleton(system)
    val shardWrite = singleton.init(SingletonActor(Product(Product.persistenceId), "Product"))
    //shardWrite ! AddProduct(9, "product 9")


  }
}