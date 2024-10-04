package ru.otus.module3

import ru.otus.module3.emailService.EmailService.Service
import zio.console.Console
import zio.{Has, ULayer, URIO, ZIO, ZLayer}


package object emailService {

    /**
     * Реализовать Сервис с одним методом sendEmail,
     * который будет принимать Email и отправлять его
     */

      // 1
     type EmailService = Has[Service]

     object EmailService {

       // 2
       trait Service{
         def sendMail(email: Email): URIO[Console, Unit]
       }


       // 3
       val live: ULayer[EmailService] = ZLayer.succeed(new Service {
         override def sendMail(email: Email): URIO[Console, Unit] =
           zio.console.putStrLn(email.toString).orDie
       })

       def sendMail(email: Email): URIO[EmailService with Console, Unit] =
         ZIO.accessM(_.get.sendMail(email))
     }

}
