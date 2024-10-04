package ru.otus.module3

import ru.otus.module3.emailService.{Email, EmailService, Html}
import ru.otus.module3.userDAO.UserDAO
import ru.otus.module3.userService.UserService.Service
import zio.console.Console
import zio.macros.accessible
import zio.{Has, RIO, URLayer, ZIO, ZLayer}

package object userService {

  /**
   * Реализовать сервис с одним методом
   * notifyUser, принимает id пользователя в качестве аргумента и шлет ему уведомление
   * при реализации использовать UserDAO и EmailService
   */

   // 1

   type UserService = Has[Service]

   @accessible
   object UserService{

     // 2
     trait Service{
       def notifyUser(id: UserID): RIO[EmailService with Console, Unit]
     }

     class ServiceImpl(userDAO: UserDAO.Service) extends Service{
       override def notifyUser(id: UserID): RIO[EmailService with Console, Unit] = for{
         user <- userDAO.findBy(UserID(1)).some
           .orElseFail(new Throwable(s"User not found - ${id.id}"))
         email = Email(user.email, Html("Hello here"))
         _ <- EmailService.sendMail(email)
       } yield ()
     }

     // 3

     val live: URLayer[UserDAO, UserService] = ZLayer.fromService[UserDAO.Service, UserService.Service](dao =>
       new ServiceImpl(dao)
     )
   }



}
