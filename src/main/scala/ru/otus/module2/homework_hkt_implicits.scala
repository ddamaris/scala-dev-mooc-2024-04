package ru.otus.module2

import scala.language.implicitConversions

object homework_hkt_implicits {

  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]

  }

  object Bindable {
    implicit def optBind[T](o: Option[T]): Bindable[Option, T] = new Bindable[Option, T] {
      override def map[G](f: T => G): Option[G] = o.map(f)
      override def flatMap[G](f: T => Option[G]): Option[G] = o.flatMap(f)
    }

    implicit def strBind[T](o: List[T]): Bindable[List, T] = new Bindable[List, T] {
      override def map[G](f: T => G): List[G] = o.map(f)
      override def flatMap[G](f: T => List[G]): List[G] = o.flatMap(f)
    }
  }

  def tuplef[F[_], A, B](fa: F[A], fb: F[B])(implicit ba: F[A] => Bindable[F, A], bb: F[B] => Bindable[F, B]): F[(A, B)] = {
    fa.flatMap(a => fb.map(b => (a, b)))
  }

  val res_1: Option[(Int, String)] = tuplef[Option, Int, String](Some(5), Some("asd"))
  val res_2: List[(Int, String)] = tuplef[List, Int, String](List(5, 6), List("a", "b", "c"))
  println(res_1)
  println(res_2)
}