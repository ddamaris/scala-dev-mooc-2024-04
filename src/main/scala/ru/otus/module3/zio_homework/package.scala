package ru.otus.module3

import zio.{Has, RIO, ULayer, URIO, ZIO, ZLayer}
import zio.console.{Console, getStrLn, putStrLn}
import zio.random._
import zio.clock.{Clock, currentTime, sleep}
import zio.duration.durationInt

import scala.concurrent.duration.MILLISECONDS
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  lazy val guessProgram: RIO[Console with Random, Unit] = for {
    _ <- putStrLn("Введите число от 1 до 3:")
    num <- getStrLn.map(_.toInt)
    randomNum <- nextIntBetween(1, 4)
    _ <- putStrLn(if (num == randomNum) "Угадал :)" else "Не угадал :(")
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile [R, E](effect : ZIO[R, E, Boolean]): ZIO[R, E, Boolean] = {
    effect.repeatWhile(!_)
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault: RIO[Console, config.AppConfig] = {
    config.load.orElse {
      for {
        defaultConfig <- ZIO.effect(config.AppConfig("defaultHost", "port"))
        _ <- putStrLn(defaultConfig.toString)
      } yield defaultConfig
    }
  }


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val effect: URIO[Random with Clock, Int] = for {
    _ <- sleep(1 second)
    randomNum <- nextIntBetween(0, 11)
  } yield randomNum

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: Iterable[ZIO[Random with Clock, Nothing, Int]] = ZIO.replicate(10)(effect)

  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: RIO[Console with Clock with Random, Int] =
    zioConcurrency.printEffectRunningTime(
      for {
        sum <- ZIO.mergeAll(effects)(0)(_ + _)
        _ <- zio.console.putStrLn(s"sum: $sum")
      } yield sum
    )


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp =
    zioConcurrency.printEffectRunningTime(
      for {
        sum <- ZIO.mergeAllPar(effects)(0)(_ + _)
        _ <- zio.console.putStrLn(s"sum: $sum")
      } yield sum
    )

  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.console.putStrLn например
   */

  type PrintEffectRunningTime = Has[PrintEffectRunningTime.Service]

  object PrintEffectRunningTime {
    trait Service {
      def printEffectRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[PrintEffectRunningTime with Console with Clock with R, E, A]
    }

    case class ServiceImpl() extends Service {
      override def printEffectRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[PrintEffectRunningTime with Console with Clock with R, E, A] =
        for {
          start <- currentTime(MILLISECONDS)
          r <- effect
          end <- currentTime(MILLISECONDS)
          _ <- putStrLn(s"Running time ${end - start}").orDie
        } yield r
    }

    val live: ULayer[PrintEffectRunningTime] = ZLayer.succeed(ServiceImpl())

    def printRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[PrintEffectRunningTime  with Console with Clock with R, E, A] = {
      ZIO.accessM(_.get.printEffectRunningTime(effect))
    }
  }

  /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val sumOfRandoms: RIO[Console with Random with Clock, Int] =
    for {
      sum <- ZIO.mergeAllPar(effects)(0)(_ + _)
      _ <- zio.console.putStrLn(s"sum: $sum")
    } yield sum

  lazy val appWithTimeLogg: RIO[PrintEffectRunningTime with Console with Clock with Random, Int] =
    PrintEffectRunningTime.printRunningTime(sumOfRandoms)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp: RIO[Console with Clock with Random, Int] =
    appWithTimeLogg.provideSomeLayer[Console with Random with Clock](PrintEffectRunningTime.live)

}
