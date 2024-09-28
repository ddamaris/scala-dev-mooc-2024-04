package ru.otus.module3

import zio.duration.durationInt
import zio.random._
import zio.{Schedule, UIO, ZIO}

import java.util.concurrent.atomic.AtomicReference
import scala.language.postfixOps

object zioDS {

  object schedule {

    val eff = ZIO.effect(println("hello"))

    /** 1. Написать эффект, который будет выводить в консоль Hello 5 раз
     */

    val schedule1: Schedule[Any, Any, Long] = Schedule.recurs(5)

    val eff1 = eff.repeat(schedule1)


    /** 2. Написать эффект, который будет выводить в консоль Hello 5 раз, раз в секунду
     */

    val schedule2 = Schedule.fixed(1 second)

    val eff2 = eff.repeat(schedule1 && schedule2)

    /** Написать эффект, который будет генерить произвольное число от 0 до 10,
     * и повторяться пока число не будет равным 0
     */

    val schedule3 = Schedule.recurWhile[Int]( _ > 0)

    val random = nextIntBetween(0, 11)

    val eff3 = random.repeat(schedule3)


    /** Написать планировщик, который будет выполняться каждую пятницу 12 часов дня
     */

    val schedule5 = Schedule.dayOfWeek(5) && Schedule.hourOfDay(12)

  }

  object ref {

    /**
     * Счетчик
     *
     */

    var counter: Int = 0

    val updateCounter = UIO.foreachPar((1 to 5).toList){ _ =>
      ZIO.effectTotal(counter += 1)
    }.as(counter)



    trait Ref[A] {
      def modify[B](f: A => (B, A)): UIO[B]

      def get: UIO[A] = modify(a => (a, a))

      def set(a: A): UIO[Unit] = modify(_ => ((), a))

      def update[B](f: A => A): UIO[Unit] =
        modify(a => ((), f(a)))
    }

    object Ref{
      def make[A](a: A): UIO[Ref[A]] = ZIO.effectTotal{
        new Ref[A]{
          val atomic = new AtomicReference(a)
          override def modify[B](f: A => (B, A)): UIO[B] = ZIO.effectTotal{
            var cond = true
            var r: B = null.asInstanceOf[B]

            while(cond){
              val current = atomic.get
              val tuple = f(current)
              r = tuple._1
              cond = !atomic.compareAndSet(current, tuple._2)
            }
            r
          }
        }
      }
    }

    lazy val updateCounterRef = for{
      counter <- Ref.make(0)
      _ <- ZIO.foreachPar((1 to 5).toList)(_ => counter.update(_ + 1))
      res <- counter.get
    } yield res


  }
}
