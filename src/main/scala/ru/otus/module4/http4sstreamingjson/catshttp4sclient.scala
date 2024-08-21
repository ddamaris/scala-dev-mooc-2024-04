package ru.otus.module4.http4sstreamingjson

import org.http4s.ember.client.EmberClientBuilder
import ru.otus.module4.catsmiddleware.Restfull
import cats.effect.{IO,IOApp,Resource}
import org.http4s.client.Client
import org.http4s.{Request, Response, Uri}
import cats.effect

object HttpClient {
  val builder: Resource[IO, Client[IO]] = EmberClientBuilder.default[IO].build
  val request = Request[IO](uri = Uri.fromString("http://localhost:8081/hello").toOption.get)

  //1
  val result: Resource[IO, Response[IO]] = for {
    client <- builder
    response <- client.run(request)
  } yield response

  //2
  val result1: Resource[IO, String] = for {
    client <- builder
    response <- effect.Resource.eval(client.expect[String](request))
  } yield response

  //3
  val result3 = builder.use(
    client => client.run(request).use(
      resp =>
        if (!resp.status.isSuccess)
          resp.body.compile.to(Array).map(new String(_))
        else
          IO("error")
    )
  )
}

object mainServer extends IOApp.Simple {
  def run(): IO[Unit] = {
    // for 1
/*    for {
      fiber <- Restfull.serverSessionAuthServerClear.use(_ => IO.never).start
      _ <- HttpClient.result.use(IO.println)
      _ <- fiber.join
    } yield ()
    */
    // for 2
 /*   for {
      fiber <- Restfull.serverSessionAuthServerClear.use(_ => IO.never).start
      _ <- HttpClient.result1.use(IO.println)
      _ <- fiber.join
    } yield ()*/
    // for 3
    Restfull.serverSessionAuthServerClear.use(_ => HttpClient.result3.flatMap(IO.println) *> IO.never)

  }
}
