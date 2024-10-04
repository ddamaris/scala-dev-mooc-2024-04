package ru.otus.module4.catsstreaming

import cats.effect.kernel.Async
import cats.effect.std.Queue
import cats.effect.{IO, IOApp, Resource}
import fs2.{Chunk, Pure, Stream, io, text}

import java.nio.file.Paths
import java.time.Instant
import scala.concurrent.duration._

object Streams extends IOApp .Simple {
  // 1.
  val pureApply: Stream[Pure, Int] = Stream.apply(1,2,3)

  // 2.
  val ioApply: Stream[IO, Int] = pureApply.covary[IO]

  //3.
  val list = List(1,2,3,4)
  val strm1: Stream[Pure, Int] = Stream.emits(list)

  //4
  val a: List[Int] = pureApply.toList
  val aa: IO[List[Int]] = ioApply.compile.toList

  //5
  val unfoalded: Stream[IO, String] = Stream.unfoldEval(0) { s =>
    val next = s + 10
    if (s >= 50) IO.none
    else IO.println(next.toString).as(Some((next.toString, next)))
  }

  //6
  val s = Stream.eval(IO.readLine).evalMap(s=> IO.println(s">>$s")).repeatN(3)

  //7
  type Description = String
  def openFile: IO[Description] = IO.println("open file").as("file description")
  def closeFile(desc: Description): IO[Unit] = IO.println("closing file")
  def readFile(desc: Description): Stream[IO, Byte] = Stream.emits(s"File content".map(_.toByte).toArray)

  val fileResource: Resource[IO, Description] = Resource.make(openFile)(closeFile)
  val resourceStream: Stream[IO, Int] = Stream.resource(fileResource).flatMap(readFile).map(b=> b.toInt + 100)

  //9
  def writeToSocket[F[_]: Async](chunk: Chunk[String]): F[Unit] =
    Async[F].async_{ callback =>
      println(s"[thread: ${Thread.currentThread().getName}] :: Writing $chunk to socket")
      callback(Right())
    }

  //10
  val fixedDelayStream = Stream.fixedDelay[IO](1.second).evalMap(_ => IO.println(Instant.now))
  val fixedRateStream = Stream.fixedRate[IO](1.second).evalMap(_ => IO.println(Instant.now))


  //11
  val queueIO = cats.effect.std.Queue.bounded[IO, Int](100)
  def putInQueue(queue: Queue[IO, Int], value: Int) =
    queue.offer(value)

  val queueStreamIO: IO[Stream[IO, Int]] = for {
    q <- queueIO
    _ <- (IO.sleep(5.millis) *> putInQueue(q, 5)).replicateA(10).start
  } yield Stream.fromQueueUnterminated(q)

  val queueStream: Stream[IO, Int] = Stream.force(queueStreamIO)

  val queueIO1 = cats.effect.std.Queue.bounded[IO, Int](100)
  def putInQueue1(queue: Queue[IO, Int], value: Int) =
    queue.offer(value)

  val queueStreamIO1: IO[Stream[IO, Int]] = for {
    q <- queueIO1
    _ <- (IO.sleep(5.millis) *> putInQueue(q, 5)).replicateA(10).start
  } yield Stream.fromQueueUnterminated(q)
  val queueStream1: Stream[IO, Int] = Stream.force(queueStreamIO1)

  def inc(s: Stream[IO, Int]): Stream[IO, Int] = s.map(_+1)
  def mult(s: Stream[IO, Int]): Stream[IO, Int] = s.map(_ * 10)

  def run: IO[Unit] ={
    //5
//    unfoalded.compile.drain
    //6
    //s.compile.drain
    //7
//    resourceStream.evalMap(IO.println).compile.drain
    //8 chunks
     //Stream((1 to 100): _*).chunkN(10).map(println).compile.drain
    //9
    /*Stream((1 to 100).map(_.toString): _*)
      .chunkN(10)
      .covary[IO]
      .parEvalMapUnordered(10)(writeToSocket[IO])
      .compile
      .drain*/

    //10. rate
//    fixedRateStream.compile.drain
    /*
    2024-08-07T17:53:45.116615Z
2024-08-07T17:53:46.095247400Z
2024-08-07T17:53:47.093690200Z
2024-08-07T17:53:48.103926700Z
2024-08-07T17:53:49.101190600Z
2024-08-07T17:53:50.109567500Z
2024-08-07T17:53:51.103621500Z
2024-08-07T17:53:52.099775900Z
2024-08-07T17:53:53.098742800Z
     */
    //10 delay
    //fixedDelayStream.compile.drain
    /*
    2024-08-07T17:54:29.824179500Z
2024-08-07T17:54:30.845674Z
2024-08-07T17:54:31.849840100Z
2024-08-07T17:54:32.862976400Z
2024-08-07T17:54:33.878091800Z
2024-08-07T17:54:34.888390800Z
2024-08-07T17:54:35.900376700Z
2024-08-07T17:54:36.917173Z
     */

   // (queueStream ++ queueStream1).evalMap(IO.println).compile.drain
    queueStream.through(inc).through(mult).evalMap(IO.println).compile.drain
  }
}


object  Fs2FileChunkExample extends IOApp.Simple {
  val inputFilePath = Paths.get("E://input.txt")
  val outputFilePath = Paths.get("E://output.txt")

  val fileProcessingStream: Stream[IO, Unit] = {
    io.file.Files[IO]
      .readAll(inputFilePath, 4096)
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .chunkN(2)
      .map(chunk => chunk.map(line => s"Processed: $line"))
      .flatMap(chunk => Stream.emits(chunk.toList))
      .intersperse("\n")
      .through(text.utf8.encode)
      .through(io.file.Files[IO].writeAll(outputFilePath))
  }

  val run: IO[Unit] = fileProcessingStream.compile.drain

}