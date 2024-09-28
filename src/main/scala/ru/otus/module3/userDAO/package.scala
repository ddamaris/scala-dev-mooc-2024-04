package ru.otus.module3

import ru.otus.module3.userService.User
import ru.otus.module3.userDAO.UserDAO.Service
import ru.otus.module3.userService.UserID
import zio.{Has, Task, ULayer}


package object userDAO {

    /**
     * Реализовать сервис с двумя методами
     *  1. list - список всех пользователей
     *  2. findBy - поиск по User ID
     */


    // 1
    type UserDAO = Has[Service]

    object UserDAO{

      // 2
      trait Service{
        def list(): Task[List[User]]
        def findBy(id: UserID): Task[Option[User]]
      }

      //3
      val live: ULayer[UserDAO] = ???
    }


}
