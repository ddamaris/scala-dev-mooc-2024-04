package ru.otus.module4.catsmiddleware
import cats.data.Reader
import scala.concurrent.duration._
import scala.concurrent.Await
//case class Reader[R,A](run: R=>A)

object catsmiddlewareReader {
  def main(args: Array[String]): Unit ={
    case class Config(dbUrl: String, dbUSer: String, dbPassword:String)

    val dbUrlreader: Reader[Config, String] = Reader(config => config.dbUrl)
    val dbPasswordreader: Reader[Config, String] = Reader(config => config.dbPassword)
    val dbUserreader: Reader[Config, String] = Reader(config => config.dbPassword)

    val fullInfo: Reader[Config, String] = for {
      url <- dbUrlreader
      user <- dbUserreader
      password <- dbPasswordreader
    } yield s"Db url: $url, user: $user, password: $password"

    val config = Config("test","test","test")
    val result = fullInfo.run(config)
    println(result)

  }
}

import cats.data.Kleisli
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global

object catsmiddlewareKleisli {

  def getUserById(id: Int): Future[Option[String]] = Future.successful(Some("test"))
  def getOrderByUserName(name: String): Future[Option[String]] = Future.successful(Some("test1"))

  val getUserK: Kleisli[Future, Int, Option[String]] = Kleisli(getUserById)
  val getOrderK: Kleisli[Future, String, Option[String]] = Kleisli(getOrderByUserName)



  def main(args: Array[String]): Unit = {

    val getuserOrderK: Kleisli[Future, Int, Option[String]] = getUserK.flatMap{
      case Some(name) => Kleisli.liftF(getOrderK.run(name))
      case None => Kleisli.liftF[Future, Int, Option[String]](Future.successful(None))
    }

    val resultFuture: Future[Option[String]] = getuserOrderK.run(1)

    resultFuture.foreach{
      case Some(order) => println(s"Order: $order")
      case None => println("No order found")
    }

    Await.result(resultFuture, 10.seconds)
  }
}