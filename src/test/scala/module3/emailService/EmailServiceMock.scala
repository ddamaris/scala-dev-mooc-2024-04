package module3.emailService

import ru.otus.module3.emailService.{Email, EmailService}
import zio.console.Console
import zio.{Has, URIO, URLayer, ZLayer}
import zio.test.mock

object EmailServiceMock extends mock.Mock[EmailService]{

  object SendMail extends Effect[Email, Nothing, Unit]

  val compose: URLayer[Has[mock.Proxy], EmailService] = ZLayer.fromService{ proxy =>
    new EmailService.Service {
      override def sendMail(email: Email): URIO[Console, Unit] = proxy(SendMail, email)
    }
  }
}
