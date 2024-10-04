package ru.otus.module4.catshtt4sdsl

import cats.effect.{IO, IOApp}
import org.http4s.{Http, HttpRoutes}
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import com.comcast.ip4s.{Host, Port}
import org.http4s.server.Router


object Restfull {
  val service: HttpRoutes[IO] = HttpRoutes.of{
    case GET -> Root / "hello" / name => Ok(name)
  }

  val httpApp: Http[IO, IO] = service.orNotFound

  val server1 = for {
    s <- EmberServerBuilder
      .default[IO]
      .withHost(Host.fromString("localhost").get)
      .withPort(Port.fromInt(8080).get)
      .withHttpApp(httpApp).build
  } yield s
}

object mainServer extends IOApp.Simple {
  def run(): IO[Unit] = {
    Restfull.server1.use( _ => IO.never)
  }
}

//2
object RestFull2Endpoints {
  val serviceOne: HttpRoutes[IO] =
    HttpRoutes.of{
      case GET -> Root/ "hello1"/name => Ok(s"web service from $name")
      case POST -> Root/ "hello2"/name => Ok(s"web service from $name")
    }

  val serviceTwo: HttpRoutes[IO] = {
    HttpRoutes.of {
      case GET -> Root/"hello2"/name=> Ok("web service Ok2")
    }
  }

  val service: HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root / "hello" / name => Ok("web service 0")
  }

  val router = Router(
    "/"->serviceOne,
    "/api" -> serviceTwo,
    "/apiroot" -> service
  )

  val server = EmberServerBuilder
    .default[IO]
    .withHost(Host.fromString("localhost").get)
    .withPort(Port.fromInt(8081).get)
    .withHttpApp(router.orNotFound).build
}

object mainServer3Services extends IOApp.Simple {
  def run(): IO[Unit] = {
    RestFull2Endpoints.server.use(_ => IO.never)
  }
}