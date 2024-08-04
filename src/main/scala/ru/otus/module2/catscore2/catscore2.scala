package ru.otus.module2.catscore2
import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._


object catscore2 extends  App{
  val showInt = 123.show
  println(showInt)

  val showString = "hello".show
  println(showString)

  case class Person(name: String, age: Int)

  implicit val showPreson: Show[Person] = new Show[Person] {
    def show(p: Person): String = "sdf"
  }

  val person = Person("sdf",30)
  println(person.show)


  trait Serializer[T] {
    def serialize(value: T):String
  }

  object SerializerInstances {
    implicit val intSerializer: Serializer[Int] = new Serializer[Int] {
      def serialize(value:Int): String = value.toString
    }
    implicit val stringSerializer: Serializer[String] = new Serializer[String] {
      def serialize(value:String): String = value
    }

    implicit val personSerializer: Serializer[Person] = new Serializer[Person] {
      def serialize(p: Person): String = "sdf"
    }
  }

  // main scala type classes
  //1. Functor
  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
  }

  //2. Monada
  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A,B](fa: F[A])(f: A=>F[B]): F[B]
    def pure[A](a:A): F[A]
  }

  //3. Applicative
  trait Applicative[F[_]] extends Functor[F] {
    def ap[A,B](ff: F[A=>B])(fa: F[A]): F[B]
    def pure[A](a:A):F[A]
  }

  //4. Semigroup

  trait Semigroup[A] {
    def combine(a:A, y:A):A
  }

  //5. Monoid
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }
}
