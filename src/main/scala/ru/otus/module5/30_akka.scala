package ru.otus.module5

import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Props, SpawnProtocol}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import ru.otus.module5.simpleActors.{Echo0, Supervisor}
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout
import ru.otus.module5.simpleActors.change_behaviour.Worker
import ru.otus.module5.simpleActors.change_behaviour.Worker.WorkerProtocol

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

object simpleActors{

  object Echo0{
    def apply(): Behavior[String] = Behaviors.setup[String]{ ctx =>
      Behaviors.receiveMessage[String]{
        case msg =>
          ctx.log.info(ctx.self.toString)
          ctx.log.info(msg)
          Behaviors.same
      }
    }
  }

  class Echo(ctx: ActorContext[String]) extends AbstractBehavior[String](ctx){
    override def onMessage(msg: String): Behavior[String] = {
      ctx.log.info(msg)
      this
    }
  }

  object Echo{
    def apply(): Behavior[String] = Behaviors.setup[String](ctx => new Echo(ctx))
  }

  object Supervisor{
    def apply(): Behavior[SpawnProtocol.Command] = Behaviors.setup{ ctx =>
      ctx.log.info(ctx.self.toString)
      SpawnProtocol()
    }
  }

  object change_behaviour{
    object Worker{
      sealed trait WorkerProtocol
      object WorkerProtocol{
        case object Start extends WorkerProtocol
        case object StandBy extends WorkerProtocol
        case object Stop extends WorkerProtocol
      }

      import WorkerProtocol._
      def apply(): Behavior[WorkerProtocol] = idle()

      def idle(): Behavior[WorkerProtocol] = Behaviors.setup{ ctx =>
        Behaviors.receiveMessage{
          case Start =>
            ctx.log.info("Start")
            workInProgress()
          case StandBy =>
            ctx.log.info("Stand by")
            idle()
          case Stop =>
            ctx.log.info("Stop")
            Behaviors.stopped
        }
      }
      def workInProgress(): Behavior[WorkerProtocol] = Behaviors.setup{ctx =>
        Behaviors.receiveMessage {
          case WorkerProtocol.Start =>
            workInProgress()
          case WorkerProtocol.StandBy =>
            ctx.log.info("Going stand by")
            idle()
          case WorkerProtocol.Stop =>
            Behaviors.stopped
        }
      }
    }
  }
}

object ActorApp{
  def main(args: Array[String]): Unit = {
    // val system = ActorSystem(Echo0(), "Echo0")

    implicit val timeout = Timeout(1 seconds)
    implicit val system: ActorSystem[SpawnProtocol.Command] = ActorSystem(Supervisor(), "Supervisor")
    implicit val ec = system.executionContext

//    val futureEchoRef: Future[ActorRef[String]] =
//      system.ask(SpawnProtocol.Spawn(Echo0(), "Echo0", Props.empty, _))
//
//    futureEchoRef.foreach{ ref =>
//      ref ! "Hello world"
//    }
    val futureWorkerRef = system.ask[ActorRef[WorkerProtocol]](
      SpawnProtocol.Spawn(Worker(), "Worker", Props.empty, _))

    futureWorkerRef.foreach{ ref =>
      ref ! WorkerProtocol.Start
      ref ! WorkerProtocol.Start
      ref ! WorkerProtocol.StandBy
      ref ! WorkerProtocol.Stop
    }

    Thread.sleep(3000)
    system.terminate()
  }
}