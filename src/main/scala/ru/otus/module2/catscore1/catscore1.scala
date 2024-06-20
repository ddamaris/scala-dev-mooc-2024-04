package ru.otus.module2.catscore1

import cats.data.{Chain, Ior, NonEmptyChain, NonEmptyList, Validated, ValidatedNec}
import cats.implicits._


object dataStructure {
  //1. chain
  val empty: Chain[Int] = Chain[Int]()
  val empty2: Chain[Int] = Chain.empty[Int]

  val ch2 = Chain(1)
  val ch3 = Chain.one(1)
  val ch4 = Chain.fromSeq(1::2::3::4::5::Nil)

  //operators
  val ch5 = ch2 :+ 2
  val ch6 = 3 +: ch2
  val r = ch2.headOption
  ch3.map(_+1)
  ch3.flatMap(x=>Chain.one(x+1))


  // nonemptychain
  val nec = NonEmptyChain(1)
  val nec1 = NonEmptyChain.one(1)
  val nec2: Option[NonEmptyChain[Int]] = NonEmptyChain.fromSeq(1::2::3::Nil)
  val r2 = nec.head

  //NonEmptyList
  val nel1 = NonEmptyList(1, List())
  val nel2 = NonEmptyList.one(1)
  val nel3: Option[NonEmptyList[Int]] = NonEmptyList.fromList(1::Nil)

  //Monads
  import cats.Monad
  import cats.instances.option._
  val option1 = Some(1)
  val option2 = Monad[Option].flatMap(option1)(x=> Some(x+1))

  //Semigroup
  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._

  val combineInt = 1 |+| 2

  //Monoid
  import cats.Monoid
  import cats.instances.string._
  val combineString = "1" |+| "2" // 12

  //Option
  val someValue: Option[Int] = Some(42)
  import cats.syntax.option._
  val someValue1 = 42.some

  //Either
  import cats.syntax.either._

  val rightValue: Either[String, Int] = 42.asRight[String]
}

object validation {
  type EmailValidationError = String
  type NameValidationError = String
  type AgeValidationError = String

  type Name = String
  type Email = String
  type Age = Int

  case class UserDTO(email: String, name: String, age: Int)
  case class User(email:String, name: String, age: Int)

  // 1.
  def emailValidationE: Either[EmailValidationError, Email] = Left("not valid email")
  def userNameValidationE: Either[NameValidationError, Name] = Left("Not valid user name")
  def ageValidationE: Either[AgeValidationError, Age] = Right(30)

  def validateUserDataE(userDto: UserDTO): Either[String, User] = for {
    email <- emailValidationE
    name <- userNameValidationE
    age <- ageValidationE
  } yield User(email, name, age)

  // cats validation
  //2.
  val v1 = Validated.invalid[String, String]("sdf")
  val v2 = Validated.valid[String, String]("sdg")

  def emailValidationV: Validated[EmailValidationError, Email] = "email not valid".invalid[String]
  def userNameValidationV: Validated[NameValidationError, Name] = "name not valid".invalid[String]
  def userAgeValidationV: Validated[AgeValidationError, Age] = 30.valid[String]

  /*def validateUserDataV(userDTO: UserDTO): Validated[String, User] = for {
    email <- emailValidationV
    name <- userNameValidationV
    age <- userAgeValidationV
  } yield (email, name, age)*/

  def validateUserDataV(user:UserDTO): Validated[String, String] =
    emailValidationV combine userNameValidationV combine userAgeValidationV.map(_.toString)

  // 3. improvement
  def validateUserDataV2(userDto: UserDTO): ValidatedNec[String, User] = (
    emailValidationV.toValidatedNec,
    userNameValidationV.toValidatedNec,
    userAgeValidationV.toValidatedNec
  ) .mapN {
    (email, name, age) =>
      User(email, name, age)
  }

  // 4. Ior
  val u:User = User("a", "b", 30)
  lazy val ior: Ior[String, User] = Ior.left("")
  lazy val ior1: Ior[String, User] = Ior.right(u)
  lazy val ior2: Ior[String, User] = Ior.both("warning", u)

  def emailValidationI: Ior[String, String] = Ior.both("email ???", "sdfsdf@zsdfdsf.de")
  def userNameValidationI: Ior[String, String] = Ior.both("name ???", "sdsdbg")
  def userAgeValidationI: Ior[String, Int] = 30.rightIor[String]

  def validateUserDataI(userDto:UserDTO): Ior[String, User] = for {
    email <- emailValidationI
    name <- userNameValidationI
    age <- userAgeValidationI
  } yield User(email, name, age)

  //5
  def validateUserDataI2(userDto:UserDTO): Ior[NonEmptyChain[String], User] = for {
    email <- emailValidationI.toIorNec
    name <- userNameValidationI.toIorNec
    age <- userAgeValidationI.toIorNec
  } yield User(email, name, age)



  def main(args: Array[String]): Unit = {
    //println(validateUserDataE(UserDTO("","",20))) //for 1
    // println(validateUserDataV(UserDTO("","",20))) //for 2
    // println(validateUserDataV2(UserDTO("","",20))) for 3
    // println(validateUserDataI(UserDTO("","",20))) for 4
    println(validateUserDataI2(UserDTO("","",20)))
  }



}

import cats.data.Kleisli
import cats.instances.future._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object kleisliusage extends  App{
  // A => F[B], F - тип монады
  val f: Int => Future[Int] = x => Future.successful(x+1)
  val g: Int => Future[Int] = x => Future.successful(x*2)

  val kleisliF = Kleisli(f)
  val kleisliG = Kleisli(g)

  val combined = kleisliF andThen kleisliG

  val result: Future[Int] = combined.run(10)
  result.foreach(println)



}