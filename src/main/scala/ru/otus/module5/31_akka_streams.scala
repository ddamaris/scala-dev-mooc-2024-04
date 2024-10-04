package ru.otus.module5

import akka.{Done, NotUsed}
import akka.actor.{ActorRef, ActorSystem}
import akka.stream.Supervision.Resume
import akka.stream.{ActorAttributes, Attributes, ClosedShape, CompletionStrategy, FanInShape2, Graph, IOResult, OverflowStrategy, Supervision, UniformFanOutShape}
import akka.stream.scaladsl.{Broadcast, FileIO, Flow, Framing, GraphDSL, Keep, RunnableGraph, Sink, Source, Zip}
import akka.util.ByteString
import ru.otus.module5.streams.{Person, csvSink, csvSource, lineFlow, parseRow, s0, s4, sink1, system}

import scala.language.postfixOps
import java.nio.file.Paths
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object streams {

  case class Person(id: Int, fio: String)

  val system: ActorSystem = ActorSystem("simple-streams")
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

  val flow1: Flow[Int, Int, NotUsed] = Flow[Int].map(_ + 1)
  val flow2: Flow[Int, String, NotUsed] = Flow[Int].map(_.toString)

  // 1
  val csvSource: Source[ByteString, Future[IOResult]] =
    FileIO.fromPath(Paths.get("test.csv"))

  val lineFlow = Flow[ByteString]
    .via(Framing.delimiter(ByteString("\n"), 256, allowTruncation = true))

  val parseRow: Flow[ByteString, Person, NotUsed] = Flow[ByteString].drop(1).map{ bs =>
    val strRep = bs.utf8String
    val cells = strRep.split(",")
    Person(cells(0).toInt, cells(1))
  }

  val csvSink: Sink[Person, Future[Seq[Person]]] = Sink.seq[Person]

  val g1: Graph[ClosedShape.type, NotUsed] = GraphDSL.create(){ implicit b =>
    import GraphDSL.Implicits._

    val in =  FileIO.fromPath(Paths.get("test.csv"))

    val out = Sink.ignore

    val broadcast: UniformFanOutShape[Person, Person] = b.add(Broadcast[Person](2))

    val zip: FanInShape2[Person, Unit, (Person, Unit)] = b.add(Zip[Person, Unit]())

    val lFlow = Flow[ByteString]
      .via(Framing.delimiter(ByteString("\n"), 256, allowTruncation = true))

    val pFlow: Flow[ByteString, Person, NotUsed] = Flow[ByteString].drop(1).map{ bs =>
      val strRep = bs.utf8String
      val cells = strRep.split(",")
      Person(cells(0).toInt, cells(1))
    }

    val logFlow: Flow[Person, Unit, NotUsed] = Flow[Person].map(println)

    val resultFlow: Flow[(Person, Unit), Person, NotUsed] =
      Flow[(Person, Unit)].map(_._1)

    in ~> lFlow ~> pFlow ~> broadcast.in
    broadcast ~> logFlow ~> zip.in1
    broadcast ~> zip.in0
    zip.out ~> resultFlow ~> out
    ClosedShape
  }
  val rg = RunnableGraph.fromGraph(g1)

}

object StreamApp{

  def main(args: Array[String]): Unit = {

    implicit val actorSystem = ActorSystem("Streams")
    val sink2: Sink[String, Future[Done]] = Sink.foreach[String](println)
//    val stream1: RunnableGraph[ActorRef] = s4.to(sink2)
//    val ref: ActorRef = stream1.run()
//    ref ! "Hello1"
//    ref ! "Hello2"
//    ref ! "Hello3"
//    ref ! "Hello4"
//    ref ! Done
//    Thread.sleep(1000)
//    ref ! "Hello5"


//    val result =
//      streams.s0.via(streams.flow1).via(streams.flow2).to(sink2)
//    result.run()

    val result: Future[Seq[Person]] =
      csvSource.via(lineFlow).via(parseRow).toMat(csvSink)(Keep.right).run()

    println(Await.result(result, 1 seconds))
    system.terminate()
    actorSystem.terminate()
  }
}