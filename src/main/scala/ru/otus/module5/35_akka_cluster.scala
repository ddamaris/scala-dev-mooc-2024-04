package ru.otus.module5
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.sharding.typed.scaladsl.{ClusterSharding, Entity, EntityRef, EntityTypeKey, ShardedDaemonProcess}
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.typed.{ClusterSingleton, SelfUp, SingletonActor}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import ru.otus.module5.shard.ShardActor.TypeKey

import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}




object singleton {

  val config = ConfigFactory.parseString(
    """akka {
      |  actor {
      |    provider = cluster
      |    serialization-bindings {
      |      "ru.otus.module5.CborSerializable" = jackson-cbor
      |    }
      |  }
      |
      |  remote {
      |    artery {
      |      canonical.hostname = "127.0.0.1"
      |      canonical.port = 2551
      |    }
      |  }
      |
      |  cluster {
      |    seed-nodes = [
      |      "akka://ClusterSystem@127.0.0.1:2551"
      |    ]
      |
      |    sharding {
      |      number-of-shards = 100
      |    }
      |  }
      |
      |}""".stripMargin)

  sealed trait Command
  case object SingletonMessage extends Command

  // поведение
  val singletonBehaviour: Behavior[Command] = Behaviors.receive{ (context, msg) =>
    msg match {
      case SingletonMessage =>
        context.log.info("Cluster singleton receive msg")
        Behaviors.same
    }
  }

  object ClusterSingletonExample{
    def apply(): Behavior[Command] = Behaviors.setup{ ctx =>
      val singletonManager = ClusterSingleton(ctx.system)
      val singletonProxy = singletonManager.init(
        SingletonActor(singletonBehaviour, "singletonActor")
      )

      singletonProxy ! SingletonMessage
      Behaviors.same
    }
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem(ClusterSingletonExample(),"ClusterSystem", config)
  }
}

object shard{

  val config = ConfigFactory.parseString("""akka {
                                           |  actor {
                                           |    provider = cluster
                                           |    serialization-bindings {
                                           |      "ru.otus.module5.CborSerializable" = jackson-cbor
                                           |    }
                                           |  }
                                           |
                                           |  remote {
                                           |    artery {
                                           |      canonical.hostname = "127.0.0.1"
                                           |      canonical.port = 2551
                                           |    }
                                           |  }
                                           |
                                           |  cluster {
                                           |    seed-nodes = [
                                           |      "akka://ClusterSystem@127.0.0.1:2551"
                                           |    ]
                                           |
                                           |    sharding {
                                           |      number-of-shards = 100
                                           |    }
                                           |  }
                                           |
                                           |}""".stripMargin)

  sealed trait ShardCommand
  case class ShardMessage(id: String, data: String) extends ShardCommand

  object ShardActor{
    val TypeKey: EntityTypeKey[ShardCommand] = EntityTypeKey[ShardCommand]("ShardActor")

    def apply(id: String): Behavior[ShardCommand] = Behaviors.receive{ (ctx, msg) =>
      msg match {
        case ShardMessage(id, data) =>
          ctx.log.warn(s"Shard receive message [$msg]")
          Behaviors.same
      }
    }
  }

  object ClusterShardExample{
    def apply(): Behavior[String] = Behaviors.setup{ ctx =>
      val clusterSharding = ClusterSharding(ctx.system)
      clusterSharding.init(Entity(TypeKey)(ctx => ShardActor(ctx.entityId)))
      Behaviors.receiveMessage{
        case msg =>
          ctx.log.warn(s"Receive msg $msg")
          val shardRegion = clusterSharding.entityRefFor(TypeKey, "shard-1")
          shardRegion ! ShardMessage("shard-1", "Hello from cluster sharding")
          Behaviors.same
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem(ClusterShardExample(), "ClusterSystem", config)
    system ! "System Up!"
  }

}