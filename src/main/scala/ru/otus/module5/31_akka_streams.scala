package ru.otus.module5

import akka.{Done, NotUsed}
import akka.actor.{ActorRef, ActorSystem}
import akka.stream.Supervision.Resume
import akka.stream.{ActorAttributes, Attributes, ClosedShape, CompletionStrategy, IOResult, OverflowStrategy, Supervision, UniformFanOutShape}
import akka.stream.scaladsl.{Broadcast, FileIO, Flow, Framing, GraphDSL, Keep, RunnableGraph, Sink, Source, Zip}
import akka.util.ByteString
import ru.otus.module5.streams.{s0, s4, sink1}

import scala.language.postfixOps
import java.nio.file.Paths
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object streams {

  case class Person(id: Int, fio: String)

  implicit val system: ActorSystem = ActorSystem("simple-streams")
  implicit val ec = system.dispatcher


  // Создание источников



  // конечный источник чисел

  val s0: Source[Int, NotUsed] = Source(1 to 10)

  // Бесконечный источник чисел

  val s1: Source[Int, NotUsed] = Source.fromIterator[Int](() => Iterator.from(0))

  // Конечный источник на 1 элемент

  val s2: Source[Int, NotUsed] = Source.single(1)

  // конечный источник Future

  val s3: Source[Int, NotUsed] = Source.future(Future(1))



  // бесконечный источник с ссылкой на актор

  val completionMatcher: PartialFunction[Any, CompletionStrategy] = {
    case Done =>
      println("Completion triggered")
      CompletionStrategy.immediately
  }
  val s4: Source[String, ActorRef] = Source.actorRef[String](completionMatcher,
    PartialFunction.empty, 10,
    OverflowStrategy.dropHead)

  // Простой потребитель

  val sink1: Sink[Any, Future[Done]] = Sink.ignore


  // Простой flow

}

object StreamApp{
  def main(args: Array[String]): Unit = {
    implicit val actorSystem = ActorSystem("Streams")
    val sink2 = Sink.foreach[String](println)
    val stream1: RunnableGraph[ActorRef] = s4.to(sink2)
    val ref: ActorRef = stream1.run()
    ref ! "Hello1"
    ref ! "Hello2"
    ref ! "Hello3"
    ref ! "Hello4"
    ref ! Done
    Thread.sleep(1000)
    ref ! "Hello5"
    actorSystem.terminate()
  }
}