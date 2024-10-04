package ru.otus.module3

import zio.duration.Duration
import zio.{Has, IO, RIO, Task, UIO, URIO, ZIO}
import zio.duration.durationInt

import scala.language.postfixOps
import zio.clock.Clock
import zio.console.Console
import zio.random.Random

import java.io.IOException

object di {

  type Query[_]
  type DBError
  type QueryResult[_]
  type Email = String

  trait User{
    def email: String
  }


  trait DBService{
    def tx[T](query: Query[T]): IO[DBError, QueryResult[T]]
  }

  trait EmailService{
    def makeEmail(email: String, body: String): Task[Email]
    def sendEmail(email: Email): Task[Unit]
  }

  trait LoggingService{
    def log(str: String): Task[Unit]
  }

  trait UserService{
      def getUserBy(id: Int): RIO[LoggingService, User]
  }




  /**
   * Написать эффект который напечатает в консоль приветствие, подождет 5 секунд,
   * сгенерит рандомное число, напечатает его в консоль
   *   Console
   *   Clock
   *   Random
   */

//    trait Console{
//      def putStrLn(string: String): UIO[Unit]
//    }
//
//    trait Clock {
//      def sleep(duration: Duration): UIO[Unit]
//    }
//
//    trait Random{
//      def nextInt(): UIO[Int]
//    }

    val e1: ZIO[Random with Clock with Console, IOException, Unit] = for{
      console <- ZIO.environment[Console].map(_.get)
      clock <- ZIO.environment[Clock].map(_.get)
      random <- ZIO.environment[Random].map(_.get)
      _ <- console.putStrLn("Hello")
      _ <- clock.sleep(5 seconds)
      int <- random.nextInt
      _ <- console.putStrLn(int.toString)
    } yield ()


   type MyEnv = Random with Clock with Console

   val e2: ZIO[MyEnv, IOException, Unit] = e1



  lazy val getUser: ZIO[LoggingService with UserService, Throwable, User] = ZIO.environment[UserService].flatMap(_.getUserBy(10))

  lazy val sendMail: ZIO[EmailService, Throwable, Unit] = ???


  /**
   * Эффект, который будет комбинацией двух эффектов выше
   */
  lazy val combined2: ZIO[EmailService with LoggingService with UserService, Throwable, (User, Unit)] = getUser zip sendMail

  /**
   * Написать ZIO программу которая выполнит запрос и отправит email
   */
  lazy val queryAndNotify: ZIO[LoggingService with EmailService with UserService, Throwable, Unit] = for{
    userService <- ZIO.environment[UserService]
    emailService <- ZIO.environment[EmailService]
    user <- userService.getUserBy(10)
    email <- emailService.makeEmail("foo@foo.com", "Hello world")
    _ <- emailService.sendEmail(email)
  } yield ()



  lazy val services: UserService with EmailService with LoggingService = ???

  lazy val dBService: DBService = ???
  lazy val userService: UserService = ???

  lazy val emailService2: EmailService = ???

  def f(userService: UserService): UserService with EmailService with LoggingService = ???

  // provide

  lazy val e3: Task[Unit] = queryAndNotify.provide(services)


  // provide some

  lazy val e4: ZIO[UserService, Throwable, Unit] = queryAndNotify.provideSome[UserService](f)


  // provide

  lazy val e5: Task[Unit] = e4.provide(userService)



}