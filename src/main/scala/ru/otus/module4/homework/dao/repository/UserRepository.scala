package ru.otus.module4.homework.dao.repository

import io.getquill.{EntityQuery, Quoted}
import zio.{Has, ULayer, ZIO, ZLayer}
import io.getquill.context.ZioJdbc._
import ru.otus.module4.homework.dao.entity._
import ru.otus.module4.phoneBook.db

import java.sql.SQLException
import javax.sql.DataSource


object UserRepository{


    val dc = db.Ctx
    import dc._

    type UserRepository = Has[Service]

    trait Service{
        def findUser(userId: UserId): QIO[Option[User]]
        def createUser(user: User): QIO[User]
        def createUsers(users: List[User]): QIO[List[User]]
        def updateUser(user: User): QIO[Unit]
        def deleteUser(user: User): QIO[Unit]
        def findByLastName(lastName: String): QIO[List[User]]
        def list(): QIO[List[User]]
        def userRoles(userId: UserId): QIO[List[Role]]
        def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]
        def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]
        def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
    }

    class ServiceImpl extends Service{
        val userSchema: Quoted[EntityQuery[User]] = quote{querySchema[User]("""User""")}
        val userToRoleSchema: Quoted[EntityQuery[UserToRole]] = quote{querySchema[UserToRole]("""UserToRole""")}
        val roleSchema: Quoted[EntityQuery[Role]] = quote{querySchema[Role]("""Role""")}

        override def findUser(userId: UserId): QIO[Option[User]] =
            dc.run(
              userSchema
                .filter(_.id == lift(userId.id))
                .sortBy(_.firstName)
                .take(1)
              )
              .map(_.headOption)

        override def createUser(user: User): QIO[User] = dc.run(userSchema.insert(lift(user)))

        override def createUsers(users: List[User]): QIO[List[User]] =
            dc.run(liftQuery(users).foreach{ user => userSchema.insert(user)})

        override def updateUser(user: User): QIO[Unit] =
            dc.run(userSchema.filter(_.id == lift(user.id)).update(lift(user))).unit

        override def deleteUser(user: User): QIO[Unit] =
            dc.run(userSchema.filter(_.id == lift(user.id)).delete).unit

        override def findByLastName(lastName: String): QIO[List[User]] =
            dc.run(userSchema.filter(_.lastName == lift(lastName)).sortBy(_.firstName))

        override def list(): QIO[List[User]] = dc.run(userSchema)

        override def userRoles(userId: UserId): QIO[List[Role]] =
            dc.run(
                for{
                    roleId <- userToRoleSchema.filter(_.userId == lift(userId.id)).map(_.roleId)
                    role <- roleSchema.join(_.code == roleId)
                } yield role)

        override def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit] =
            dc.run(userToRoleSchema.insert(lift(UserToRole(roleCode.code, userId.id)))).unit

        override def listUsersWithRole(roleCode: RoleCode): QIO[List[User]] =
            dc.run(
                for{
                    userId <- userToRoleSchema.filter(_.roleId == lift(roleCode.code)).map(_.userId)
                    user <- userSchema.join(_.id == userId)
                } yield user
            )

        override def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]] =
            dc.run(
                  roleSchema
                    .filter(_.code == lift(roleCode.code))
                    .sortBy(_.name)
                    .take(1)
              )
              .map(_.headOption)
    }

    val live: ULayer[UserRepository] = ZLayer.succeed(new ServiceImpl)
}