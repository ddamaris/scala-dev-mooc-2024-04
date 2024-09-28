package ru.otus.module2.catscore2
import cats.{Applicative, Monoid}
import cats.instances.list._
import cats.syntax.semigroup._

//1. monoid
object monoid_usage extends App{

  implicit val intListMonoid: Monoid[List[String]] = new Monoid[List[String]]{
    def combine(x: List[String], y: List[String]): List[String] = {
      x ++ y
    }
    def empty: List[String] = List.empty
  }

  val list1 = List("1","2","3")
  val list2 = List("4","5","6")
  val combineList = list1 |+| list2
  println(combineList.mkString(","))
}
//2. functor

import cats.Functor
import cats.instances.list._
import cats.syntax.functor._
import cats.instances.option._


object functor_usage extends App{
  //1 list
  val list = List(1,2,3)
  val incremented = list.map(_+1)
  println(incremented)

  val someValue:Option[Int] = Some(2)
  val inc = someValue.map(_+1)
  println(inc)
}


// 3. monada in cats
object monada_usage extends App {

  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A,B](fa:F[A])(f: A=>F[B]): F[B]
    def tailRecM[A,B](a:A)(f: A=> F[Either[A,B]]): F[B]
  }


  import cats.Monad
  import cats.instances.option._
  import cats.syntax.applicative._
  import cats.syntax.flatMap._

  val optionMonad = Monad[Option]
  val someValue: Option[Int] = 3.pure[Option]
  val noneValue: Option[Int] = None

  val result = optionMonad.flatMap(someValue) {
    x=>  (x+2).pure[Option]
  }
  println(result)

  val failedResult = optionMonad.flatMap(noneValue) {x =>
    (x+2).pure[Option]
  }

  println(failedResult)

}

//4 Applicative
object applicative_usage extends App {
  trait  Applicative[F[_]] extends Functor[F] {
    def ap[A,B] (ff:F[A=>B])(fa: F[A]): F[B]
    def pure[A](a:A):F[A]
  }

  import cats.Applicative
  import cats.instances.option._
  import cats.syntax.applicative._
  import cats.syntax.apply._
  import cats.instances.list._

  val optionApplicative = Applicative[Option]
  val someValue: Option[Int] = 3.pure[Option]
  val someFunction: Option[Int=>Int] = ((x:Int) => x+2).pure[Option]


  val result = someFunction.ap(someValue)
  println(result)

  val noneValue: Option[Int] = None
  val failedresult = someFunction.ap(noneValue)
  println(failedresult)

//  val listApplicative = Applicative[List]

  val list = List(1,2,3)
  val functions = List((x:Int) => x+1, (x:Int) => x*2)
  val resultList  = functions.ap(list)
  println(resultList)


  // mapN, tupleN
  val option1 = Option(1)
  val option2 = Option(2)
  val option3 = Option(3)

  val res1 = (option1,option2,option3).mapN(_ + _ + _)
  println(res1)

  import cats.syntax.apply._
  val tupleResult = (option1,option2,option3).tupled
  println(tupleResult)


}