package ru.otus.module4.homework.services

import ru.otus.module4.homework.dao.entity.{Role, RoleCode, User, UserId}
import ru.otus.module4.homework.dao.repository.UserRepository
import ru.otus.module4.phoneBook.db
import zio.{Has, RIO, ZIO, ZLayer}
import zio.macros.accessible

@accessible
object UserService{
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service{
        val dc = db.Ctx
        import dc._

        def listUsers(): RIO[db.DataSource, List[User]] =
        userRepo.list()

        def listUsersDTO(): RIO[db.DataSource,List[UserDTO]] =
            for {
                users <- userRepo.list()
                dtos <- ZIO.foreach(users)(
                             user => userRepo.userRoles(UserId(user.id)).map(roles => UserDTO(user, roles.toSet))
                         )
            } yield dtos

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] =
            dc.transaction {
                for {
                    newUser <- userRepo.createUser(user)
                    _ <- userRepo.insertRoleToUser(roleCode, UserId(user.id))
                    roles <- userRepo.userRoles(UserId(user.id))
                } yield UserDTO(newUser, roles.toSet)
            }

        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] =
            for {
                users <- userRepo.listUsersWithRole(roleCode)
                dtos <- ZIO.foreach(users) (
                    user => userRepo.userRoles(UserId(user.id)).map(roles => UserDTO(user, roles.toSet))
                    )
            } yield dtos
    }

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] =
        ZLayer.fromService[UserRepository.Service, UserService.Service](userDao => new Impl(userDao))
}

case class UserDTO(user: User, roles: Set[Role])