package ru.otus.module5

import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Props, SpawnProtocol}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import ru.otus.module5.simpleActors.Supervisor
import akka.actor.typed.scaladsl.AskPattern._

import java.util.UUID
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.concurrent.duration.DurationInt



object Counter{
  sealed trait CounterProtocol
  case object Increment extends CounterProtocol
  case class CounterRequest(sender: ActorRef[CounterProtocol]) extends CounterProtocol
  case class CounterResponse(counter: Int) extends CounterProtocol

  private def counter(c: Int): Behavior[CounterProtocol] = Behaviors.setup{ ctx =>
    Behaviors.receiveMessage {
      case Increment =>
        ctx.log.info("Increment")
       counter(c + 1)
      case CounterRequest(sender) =>
        ctx.log.info("Get counter")
        sender ! CounterResponse(c)
        Behaviors.same
      case CounterResponse(counter) =>
        Behaviors.same
    }
  }

  def apply(): Behavior[CounterProtocol] = counter(0)
}

object Crawler{
  sealed trait CrawlerProtocol
  case class Save(str: String) extends CrawlerProtocol
  case class ParseUrl(url: String) extends CrawlerProtocol
  case class CrawlerWorkerResponse(msg: CrawlerWorker.CrawlerWorkerProtocol) extends CrawlerProtocol

  def apply() = Behaviors.setup[CrawlerProtocol]{ctx =>

    val crawlerWorkerAdapter = ctx.messageAdapter[CrawlerWorker.CrawlerWorkerProtocol](
      msg => CrawlerWorkerResponse(msg)
    )

    Behaviors.receiveMessage {
      case Save(str) =>
        ctx.log.info(s"Saving result: $str")
        Behaviors.same
      case ParseUrl(url) =>
        ctx.log.info(s"Parse url: $url")
        val ref = ctx.spawn(CrawlerWorker(), s"CrawlerWorker-${UUID.randomUUID()}")
        ref ! CrawlerWorker.Parse(url, crawlerWorkerAdapter)
        Behaviors.same
      case CrawlerWorkerResponse(msg) => msg match {
        case CrawlerWorker.ParseDone(workerRef, result) =>
          ctx.self ! Save(result)
          ctx.log.info(s"Work done. Stopping worker - ${workerRef}")
          ctx.stop(workerRef)
          Behaviors.same
        case CrawlerWorker.Parse(url, replyTo) =>
          Behaviors.same
      }
    }
  }
}

object CrawlerWorker {
  sealed trait CrawlerWorkerProtocol
  case class ParseDone(workerRef: ActorRef[CrawlerWorkerProtocol], result: String) extends CrawlerWorkerProtocol
  case class Parse(url: String, replyTo: ActorRef[CrawlerWorkerProtocol]) extends CrawlerWorkerProtocol

  def apply(): Behavior[CrawlerWorkerProtocol] = Behaviors.setup[CrawlerWorkerProtocol]{ ctx =>
    Behaviors.receiveMessage {
      case ParseDone(_, _) =>
        Behaviors.same
      case Parse(url, replyTo) =>
        ctx.log.info(s"${ctx.self} Parsing url: $url")
        replyTo ! ParseDone(ctx.self, "Hello from worker")
        Behaviors.same
    }
  }
}


object App{
  def main(args: Array[String]): Unit = {

    implicit val system: ActorSystem[Crawler.CrawlerProtocol] = ActorSystem(Crawler(), "Crawler")
    implicit val timeout = Timeout(1 seconds)
//    system ! Counter.Increment
//    system ! Counter.Increment
//    system ! Counter.Increment
//    system ! Counter.Increment



    system ! Crawler.ParseUrl("localhost")
    system ! Crawler.ParseUrl("localhost")
    system ! Crawler.ParseUrl("localhost")

    Thread.sleep(2000)
    system.terminate()
  }
}