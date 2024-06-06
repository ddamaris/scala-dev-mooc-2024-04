package ru.otus.module2

object homework_hkt_implicits{

  trait Bindable[F[_], A]{
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  def tuplef[F[_], A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???
}