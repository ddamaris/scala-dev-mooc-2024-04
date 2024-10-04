package ru.otus.module3

import zio.{IO, Ref, Task, UIO, URIO, ZIO, clock}
import zio.clock.{Clock, sleep}
import zio.console.{Console, putStrLn}
import zio.duration.durationInt
import zio.internal.{Executor, ZIOSucceedNow}

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.language.postfixOps


object zioConcurrency {


  // эффект содержит в себе текущее время
  val currentTime: URIO[Clock, Long] = clock.currentTime(TimeUnit.SECONDS)


  /**
   * Напишите эффект, который будет считать время выполнения любого эффекта
   */


    // 1. Получить время
    // 2. выполнить эффект
    // 3. получить время
    // 4. вывести разницу
    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A] = for{
      start <- currentTime
      r <- zio
      end <- currentTime
      _ <- putStrLn(s"Running time ${end - start}").orDie
    } yield r


  val exchangeRates: Map[String, Double] = Map(
    "usd" -> 76.02,
    "eur" -> 91.27
  )

  /**
   * Эффект который все что делает, это спит заданное кол-во времени, в данном случае 1 секунду
   */
  lazy val sleep1Second = ZIO.sleep(1 seconds)

  /**
   * Эффект который все что делает, это спит заданное кол-во времени, в данном случае 3 секунды
   */
  lazy val sleep3Seconds = ZIO.sleep(3 seconds)

  /**
   * Создать эффект который печатает в консоль GetExchangeRatesLocation1 спустя 3 секунды
   */
  lazy val getExchangeRatesLocation1 = sleep3Seconds zipRight
    putStrLn("GetExchangeRatesLocation1")

  /**
   * Создать эффект который печатает в консоль GetExchangeRatesLocation2 спустя 1 секунду
   */
  lazy val getExchangeRatesLocation2 = sleep1Second zipRight
    putStrLn("GetExchangeRatesLocation2")



  /**
   * Написать эффект который получит курсы из обеих локаций
   */
  lazy val getFrom2Locations: ZIO[Console with Clock, IOException, (Unit, Unit)] = for{
    e1 <- getExchangeRatesLocation1
    e2 <- getExchangeRatesLocation2
  } yield (e1, e2)

  /**
   * Написать эффект который получит курсы из обеих локаций параллельно
   */
  lazy val getFrom2Locations2: ZIO[Console with Clock, IOException, (Unit, Unit)] = for{
    f1 <- getExchangeRatesLocation1.fork
    f2 <- getExchangeRatesLocation2.fork
    e1 <- f1.join
    e2 <- f2.join
  } yield (e1, e2)


  /**
   * Предположим нам не нужны результаты, мы сохраняем в базу и отправляем почту
   */


   lazy val writeUserToDB = sleep3Seconds zipRight putStrLn("Write to DB")

   lazy val sendMail = sleep1Second zipRight putStrLn("Send mail")

  /**
   * Написать эффект который сохранит в базу и отправит почту параллельно
   */

  lazy val writeAndSend: URIO[Console with Clock, Unit] = for{
    _ <- writeUserToDB.fork
    _ <- sendMail.fork
  } yield ()


  /**
   *  Greeter
   */

  lazy val greeter: ZIO[Console with Clock, IOException, Nothing] =
    (ZIO.sleep(1 seconds) zipRight putStrLn("Hello")).forever

  lazy val g1: ZIO[Console with Clock, Nothing, Unit] = for{
    f1 <- (ZIO.effect(while (true) println("Hello")).orDie).fork
    _ <- f1.interrupt
  } yield ()


  /***
   * Greeter 2
   * 
   * 
   * 
   */


 lazy val greeter2 = ???
  

  /**
   * Прерывание эффекта
   */

   lazy val app3 = ???





  /**
   * Получение информации от сервиса занимает 1 секунду
   */
  def getFromService(ref: Ref[Int]) = ???

  /**
   * Отправка в БД занимает в общем 5 секунд
   */
  def sendToDB(ref: Ref[Int]): ZIO[Clock, Exception, Unit] = ???


  /**
   * Написать программу, которая конкурентно вызывает выше описанные сервисы
   * и при этом обеспечивает сквозную нумерацию вызовов
   */

  
  lazy val app1 = ???

  /**
   *  Concurrent operators
   */


  lazy val p1 = getExchangeRatesLocation1 zipPar getExchangeRatesLocation2

  lazy val p2 = getExchangeRatesLocation1 race getExchangeRatesLocation2

  lazy val p3 = ZIO.foreachPar(List(1, 2, 3, 4, 5))(i =>
    sleep1Second zipRight putStrLn(i.toString))


  /**
   * Lock
   */


  // Правило 1
  lazy val doSomething: UIO[Unit] = ???
  lazy val doSomethingElse: UIO[Unit] = ???

  lazy val executor: Executor = ???

  lazy val eff = for{
    f1 <- doSomething.fork
    _ <- doSomethingElse
    r <- f1.join
  } yield r

  lazy val result = eff.lock(executor)



  // Правило 2
  lazy val executor1: Executor = ???
  lazy val executor2: Executor = ???



  lazy val eff2 = for{
      f1 <- doSomething.lock(executor2).fork
      _ <- doSomethingElse
      r <- f1.join
    } yield r

  lazy val result2 = eff2.lock(executor)



}