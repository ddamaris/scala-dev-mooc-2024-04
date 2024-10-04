package ru.otus.module1

import ru.otus.module1.threads.{Thread1, getRatesLocation1, getRatesLocation2, getRatesLocation3, getRatesLocation4, getRatesLocation5, getRatesLocation6, getRatesLocation7, getRatesLocation8, printRunningTime}
import ru.otus.module2.implicits.{implicit_conversions, implicit_scopes}
import ru.otus.module3.functional_effects.declarativeEncoding
import ru.otus.module3.functional_effects.functionalProgram.executableEncoding
import ru.otus.module3.{toyModel, zioConcurrency, zioConstructors, zioRecursion}
import zio.{ExitCode, IO, UIO, URIO}

object App {


  def main(args: Array[String]): Unit = {
    println(s"Hello world! ${Thread.currentThread().getName}")
//    val t1 = new Thread1
//    val t2 = new Thread1
//    t1.join()
//    t1.start()
//    t2.start()

//      def rates = {
//        val v1 = getRatesLocation5
//        val v2 = getRatesLocation6
//        println(s"Sum: ${v1 + v2}")

//        val tf1 = getRatesLocation7
//        val tf2 = getRatesLocation8
//        getRatesLocation8.onComplete{v1 =>
//          getRatesLocation7.onComplete{ v2 =>
//            println(s"Sum: ${v1 + v2}")
//          }
//        }

//        val v3: threads.ToyFuture[Int] = for{
//          v1 <- getRatesLocation7
//          v2 <- getRatesLocation8
//        } yield v1 + v2
//
//        v3.onComplete(println)
//      }
//      val start = System.currentTimeMillis()
//      printRunningTime(rates)
//      val end = System.currentTimeMillis()
//      println(s"Running time: ${end - start}  ${Thread.currentThread().getName}")

//    import scala.concurrent.ExecutionContext.Implicits.global
//    def rates = {
//      val l1 = future.getRatesLocation1
//      val l2 = future.getRatesLocation2
//      l1.flatMap { v1 =>
//        l2.map{ v2 =>
//           v1 + v2
//        }
//      }
//    }
//    future
//
//    Thread.sleep(4000)

   // implicit_scopes
   // declarativeEncoding.interpret(declarativeEncoding.greet2)
   // toyModel.echo.run()

//    val r: UIO[Unit] = zioConstructors.z12.provide("Hello world")
    //println(zioRecursion.factorial(10000))
    println(zio.Runtime.default.unsafeRun(
      zioConcurrency.printEffectRunningTime(zioConcurrency.p3)))
  }
}

object ZIOApp extends zio.App{
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    zioConstructors.z2.exitCode
}
