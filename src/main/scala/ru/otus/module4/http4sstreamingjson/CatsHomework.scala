package ru.otus.module4.http4sstreamingjson

import cats.effect.{IO, IOApp, Resource}
import cats.implicits.catsSyntaxEitherId
import com.comcast.ip4s.{Host, Port}
import fs2.Stream
import io.circe.{Decoder, Encoder}
import io.circe.derivation.{deriveDecoder, deriveEncoder}
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.{Router, Server}
import org.http4s.{HttpRoutes, Method, Request, Uri}
import org.http4s.Method.GET
import io.circe.generic.auto._
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder

import scala.concurrent.duration.DurationInt
import scala.util.Try


object CatsHomework {

  case class Counter(counter: Int)

  implicit val counterEncoder: Encoder[Counter] = deriveEncoder
  implicit val counterDecoder: Decoder[Counter] = deriveDecoder

  private def counterService: HttpRoutes[IO] = HttpRoutes.of {
    case r@GET -> Root =>
      for {
        c <- r.as[Counter]
        response <- Ok(Counter(c.counter + 1))
      } yield response
  }

  private val slowService: HttpRoutes[IO] = {
    HttpRoutes.of {
      case GET -> Root/size/total/time =>
        (for {
          size <- validate(size)
          total <- validate(total)
          time <- validate(time)
          stream = Stream
            .range(0, total)
            .map(_ => 'x'.toByte)
            .chunkN(size, allowFewer = true)
            .covary[IO]
            .metered(time.seconds)
            .unchunks
        } yield stream)
        .fold(
          e => BadRequest(e.getMessage),
          Ok(_)
      )
    }
  }

  val router: HttpRoutes[IO] = Router(
    "/counter" -> counterService,
    "/slow" -> slowService
  )

  val server: Resource[IO, Server] = for {
    s<- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8081).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(router.orNotFound).build
  } yield s

  private def validate(v: String): Either[Throwable, Int] = {
    Try(v.toInt)
      .toEither
      .fold(
        _ => new IllegalArgumentException(s"$v is not a number").asLeft,
        v => if (v < 0) new IllegalArgumentException(s"Number $v < 0").asLeft else v.asRight)
  }

  val builder: Resource[IO, Client[IO]] = EmberClientBuilder.default[IO].build
  val request: Request[IO] = Request[IO](method = Method.GET, uri = Uri.fromString("http://localhost:8081/slow/10/1024/5").toOption.get)

  val result: IO[String] = builder.use(
    client => client.run(request).use(
      resp =>
        if (!resp.status.isSuccess)
          resp.body.compile.to(Array).map(new String(_))
        else
          IO(resp.body.toString())
    )
  )

}

object CatsHomeworkApp extends IOApp.Simple {
  def run(): IO[Unit] = {
    CatsHomework.server.use(_ => CatsHomework.result.flatMap(IO.println) *> IO.never)
  }
}