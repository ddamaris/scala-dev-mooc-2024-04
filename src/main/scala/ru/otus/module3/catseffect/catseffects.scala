package catseffect

import cats.Monad
import cats.implicits._
import cats.effect._
import cats.effect.unsafe.IORuntime

import scala.concurrent.Future

//(a+b)+c =a+(b+c)
/*
trait Semigroup[A] {
  def combine(x:A, y:A):A
}

object IntSemigroup extends Semigroup[Int] {
  def combine(x: Int, y:Int): Int = x+y
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object IntMonoid extends  Monoid[Int] {
  def combine(x: Int, y:Int): Int = x+y
  def empty: Int = 0
}

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A=>B)
}

object ListFunctor extends Functor[List] {
  def map[A,B](fa: List[A])(f: A=>B): List[B] = fa.map(f)
}*/


import cats.effect.{IO, IOApp}

object  CatsDemo extends IOApp.Simple {
  def run: IO[Unit] = {
    val io: IO[Unit] = IO {
      println("hi from cats")
    }
    io
  }
}


object catseffects {
  def main(args: Array[String]): Unit = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    implicit val runtime  = IORuntime.global

    val pur = IO.pure("pure value")
    val sideeffect = IO.delay(println("111"))
    val mistake = IO.pure(println("pure 111"))

    sideeffect.unsafeRunSync()
    sideeffect.unsafeRunSync()

    println("!!!!!!!!!!!!!")
    mistake.unsafeRunSync()
    mistake.unsafeRunSync()

    val fromEither = IO.fromEither(Left(new Exception("fail")))
    val fromFuture = IO.fromFuture(IO.delay(Future.successful(1)))

    val failing = IO.raiseError(new Exception("sdf"))

    val never = IO.never

    val future = Future(Thread.sleep(2000)).map(_ => 100)

    val async =IO.async_(
      (cb: Either[Throwable, Int] => Unit) =>
        future.onComplete(a => cb(a.toEither))
    )

//    async.unsafeRunSync()
    async.flatMap(i => IO.println(i)).unsafeRunSync()
  }
}


// main part, TF
// example, no TF - Tagless Final
object FilesAndHttpIO extends IOApp.Simple {
  def readFile(file: String): IO[String] =
    IO.pure("content of some file")

  def httpPost(url: String, body: String): IO[Unit] =
    IO.delay(println(s"Post $url : $body"))

  def run: IO[Unit] = for {
    _ <- IO.delay(println("enter file path"))
    path <- IO.readLine
    data <- readFile(path)
    _ <- httpPost("sdfsdfsdfsd.de", data)
  } yield ()
}

// example with TF
trait FileSystem[F[_]] {
  def readFile(path: String): F[String]
}

object FileSystem {
  def apply[F[_]: FileSystem]: FileSystem[F] = implicitly
}

trait HttpClient[F[_]] {
  def postData(url: String, body: String): F[Unit]
}

object HttpClient {
  def apply[F[_]: HttpClient]: HttpClient[F] = implicitly
}

trait Console[F[_]] {
  def readLine: F[String]
  def printLine(s: String): F[Unit]
}

object Console {
  def apply[F[_]: Console]: Console[F] = implicitly
}

// interpreter
object Interpreter {
  implicit  val consoleIO: Console[IO] = new Console[IO] {
    override def readLine: IO[String] = IO.readLine

    override def printLine(s: String): IO[Unit] = IO.println(s)
  }

  implicit val fileSystemIO: FileSystem[IO] = new FileSystem[IO] {
    override def readFile(path: String): IO[String] = IO.pure(s"some file with some content $path")
  }

  implicit val httpClientIO: HttpClient[IO] = new HttpClient[IO] {
    override def postData(url: String, body: String): IO[Unit] = IO.delay(println(s"post $url : $body"))
  }
}

// bring it together
object  FileAndHttpTF extends IOApp.Simple {

  def program[F[_]:Console:Monad:FileSystem:HttpClient]: F[Unit] =
    for {
      _ <- Console[F].printLine("Enter file path")
      path <- Console[F].readLine
      data <- FileSystem[F].readFile(path)
      _ <- HttpClient[F].postData("sdefsdf.de", data)
    } yield ()

  import Interpreter.httpClientIO
  import Interpreter.fileSystemIO
  import Interpreter.consoleIO
  def run: IO[Unit] = program[IO]
}