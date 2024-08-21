package ru.otus.module3.catsresources
import cats.{Monad, MonadError}
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource, Sync}
import cats.implicits.catsSyntaxApply
import cats.MonadError
import cats.data.State
import cats.effect.{IO, IOApp}
import cats.implicits._
import cats.effect.unsafe.implicits.global
import cats.effect.kernel._
import java.io.{BufferedReader, FileReader}
import scala.collection.mutable
import scala.concurrent.duration._

object catsresources extends App {
  //1. ограничение типов классов
  def readFile[F[_]: Sync](filePath: String): F[String] = {
    Sync[F].delay{
      scala.io.Source.fromFile(filePath).mkString
    }
  }

  //2. локальное ограничение побочных эффектов
  def readFile1(filePath: String): IO[String] = {
    Resource.fromAutoCloseable(IO(scala.io.Source.fromFile(filePath))).use { source =>
      IO(source.getLines().mkString)
    }
  }

  //3. управление ресурсами
  def readFile2(filePath: String): Resource[IO, String] = {
    Resource.fromAutoCloseable(IO(new BufferedReader(new FileReader(filePath)))).map { reader =>
      reader.lines().toArray.mkString
    }
  }

  //4. разделение обязянностей и компетенций
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def fetchData[F[_]: Sync: Monad]: F[String] = {
    Sync[F].delay("some data")
  }

  def processData[F[_]: Sync: Monad](data:String): F[String] = {
    Sync[F].delay(data.toUpperCase)
  }

  def program[F[_]: Sync: Monad]:F[String] = for {
    data <- fetchData[F]
    processed <- processData[F](data)
  } yield processed

  //5.  контроль и аудит
  import org.slf4j.LoggerFactory
  import cats.syntax.applicative._

  def logAndPerform[F[_]: Sync](message: String)(action: => F[Unit]): F[Unit] = {
    val logger = LoggerFactory.getLogger(getClass)
    Sync[F].delay(logger.info(message)) *> action
  }

  def exampleAction[F[_]: Sync]: F[Unit] = {
    logAndPerform("starting action"){
      Sync[F].delay(println("action performed"))
    }
  }

  val program: IO[Unit] = exampleAction[IO]
  program.unsafeRunSync()


}

object catsresources1 extends App {
  //1. Sync
  /*
  trait Sync[F[_]] extends Bracket[F, Throwable] {
    def delay[A](thunk: => A): F[A]
    def suspend[A](thunk: => F[A]): F[A]
    def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A]
    def raiseError[A](e: Throwable): F[A]
  }*/

  //2 async
  /*
  trait Async[F[_]] extends Sync[F] {
    def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
    def asyncF[A](k: (Either[Throwable, A] => Unit) => F[Unit]): F[A]
    def start[A](fa: F[A]): F[Fiber[F, A]]
  }*/

  //3 Effect
  /*
  trait Effect[F[_]] extends Async[F] {
    def runAsync[A](fa: F[A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit]
  }*/

  //4. concurrent
  /*
  trait Concurrent[F[_]] extends Async[F] {
    def start[A](fa: F[A]): F[Fiber[F, A]]
    def racePair[A, B](fa: F[A], fb: F[B]): F[Either[(A, Fiber[F, B]), (Fiber[F, A], B)]]
  }*/

  //5 Timer
  /*
  trait Timer[F[_]] {
    def clock: Clock[F]
    def sleep(duration: FiniteDuration): F[Unit]
  }*/

  //usage
  import cats.effect.{Sync, Resource}
  import java.io._

  def readFile[F[_]: Sync](filePath: String):F[String] ={
    Resource.fromAutoCloseable(Sync[F].delay(new BufferedReader(new FileReader(filePath)))).use {reader =>
      Sync[F].delay {
        val sb = new StringBuilder
        var line: String = reader.readLine()
        while (line != null){
          sb.append(line)
          line = reader.readLine()
        }
        sb.toString()
      }
    }
  }

  //1. monaderror
  val optionF = for {
    a <- Some(3)
    b <- Some(3)
    c <- Some(3)
    d <- Some(3)
  } yield a + b + c + d

  val optionF1 = for {
    a <- Right(3)
    b <- Right(3)
    c <- Left("error")
    d <- Right(3)
  } yield a + b + c + d

  type MyMonadError[F[_]] = MonadError[F, String]
  def withErrorHandling[F[_]: MyMonadError] = for {
    a <- MonadError[F, String].pure(10)
    b <- MonadError[F, String].pure(10)
    c <- MonadError[F, String].pure(10)
    d <- MonadError[F, String].pure(10)
  }yield (a+b+c+d)

  type StringError[A] = Either[String, A]
  println(withErrorHandling[StringError])

  def withErrorHandling1[F[_]: MyMonadError] = for {
    a <- MonadError[F, String].pure(10)
    b <- MonadError[F, String].pure(10)
    c <- MonadError[F, String].raiseError[Int]("fail")
    d <- MonadError[F, String].pure(10)
  }yield (a+b+c+d)
  println(withErrorHandling1[StringError])
  println(withErrorHandling1.handleError(error => 42))

  def withErrorHandling2[F[_]: MyMonadError] = for {
    a <- MonadError[F, String].pure(10)
    b <- MonadError[F, String].pure(10)
    c <- MonadError[F, String].raiseError[Int]("fail").handleError(error =>42)
    d <- MonadError[F, String].pure(10)
  } yield (a + b + c + d)
  println(withErrorHandling2)

  // 2 метод attempt
  def withErrorAttempt[F[_]: MyMonadError]: F[Either[String, Int]] =
    withErrorHandling1[F].attempt
  println(withErrorAttempt)

  // *>
  val failing = IO.raiseError(new Exception("fail"))
  failing *> IO.println("sdjfskdjf")

  val a = failing.attempt *> IO.println("111111111")
  a.unsafeRunSync()


  //mondcancel
  val justSleep = IO.sleep(1.second) *> IO.println("not cancelled")
  val justSleepAndThrow = IO.sleep(100.millis) *> IO.raiseError(new Error("error"))
  //(justSleep, justSleepAndThrow).parTupled.unsafeRunSync()

  val justSleepAndThrowUncancellable = (IO.sleep(1.second) *> IO.println("not cancelled")).uncancelable
  (justSleepAndThrow, justSleepAndThrowUncancellable).parTupled.unsafeRunSync()
}


object SpawnApp extends IOApp.Simple {
  def longRunningIO(): IO[Unit] =
    (
      IO.sleep(200.millis) *> IO.println(s"hi from thread ${Thread.currentThread()}").iterateWhile( _ => true)
    )

  def longRunningIORef(r: Ref[IO, Int]): IO[Unit] = (
    IO.sleep(200.millis) *> IO.println(s"hi from thread ${Thread.currentThread()}").iterateWhile( _ => true)
  )
/*
  def run: IO[Unit] = for {
    fiber <- Spawn[IO].start(longRunningIO)
    _ <- IO.println("the fiber has been started")
    _ <- IO.sleep(1.second)
  } yield()
  */

  def run: IO[Unit] = for {
    r <- Ref.of[IO, Int](10)
    fiber1 <- Spawn[IO].start(longRunningIORef(r))
    fiber2 <- Spawn[IO].start(longRunningIO)
    fiber3 <- Spawn[IO].start(longRunningIO)
    _ <- IO.println("the fibers has been started")
    _ <- IO.sleep(2.second)
    _ <- fiber1.cancel
    _ <- fiber2.cancel
    _ <- IO.sleep(3.second)
  } yield()

}