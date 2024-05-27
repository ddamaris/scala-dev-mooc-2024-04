package ru.otus.module1


import ru.otus.module1.utils.NameableThreads

import java.util.concurrent.{Executor, ExecutorService, Executors}
import scala.collection.mutable
import scala.language.{existentials, postfixOps}

object threads {


  // Thread

  class Thread1 extends Thread{
    override def run(): Unit = {
      Thread.sleep(100)
      println(s"Hello from ${Thread.currentThread().getName}")
    }
  }

  def printRunningTime(f: => Unit) = {
    val start = System.currentTimeMillis()
    f
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
  }

  def getRatesLocation1 = {
    Thread.sleep(1000)
    println("GetRatesLocation1")
  }

  def getRatesLocation2 = {
    Thread.sleep(2000)
    println("GetRatesLocation2")
  }

  def newThread(f: => Unit): Thread = new Thread{
    override def run(): Unit = f
  }

  def getRatesLocation3 = newThread{
    Thread.sleep(1000)
    println("GetRatesLocation3")
  }

  def getRatesLocation4 = newThread{
    Thread.sleep(2000)
    println("GetRatesLocation4")
  }

  def async[A](f: => A): A = {
    var r: A = null.asInstanceOf[A]
    val t = new Thread{
      override def run(): Unit = {
        r = f
      }
    }
    t.start()
    t.join()
    r
  }

  def getRatesLocation5: Int = async{
    Thread.sleep(1000)
    println("GetRatesLocation5")
    10
  }

  def getRatesLocation6: Int = async{
    Thread.sleep(2000)
    println("GetRatesLocation6")
    20
  }

  class ToyFuture[T]private(v: => T){
    private var r: T = null.asInstanceOf[T]
    private var isCompleted: Boolean = false
    private val q = mutable.Queue[T => _]()


    def map[B](f: T => B): ToyFuture[B] = ???

    def flatMap[B](f: T => ToyFuture[B]): ToyFuture[B] = ???

    def onComplete[U](f: T => U): Unit = {
      if(isCompleted) f(r)
      else q.enqueue(f)
    }

    private def start(executor: Executor) = {
      val t = new Runnable{
        override def run(): Unit = {
          val result = v
          r = result
          isCompleted = true
          while (q.nonEmpty){
            q.dequeue()(result)
          }
        }
      }
      executor.execute(t)
    }
  }

  object ToyFuture{
    def apply[T](f: => T)(executor: Executor): ToyFuture[T] = {
      val future = new ToyFuture[T](f)
      future.start(executor)
      future
    }
  }


  def getRatesLocation7: ToyFuture[Int] = ToyFuture{
    Thread.sleep(1000)
    println(s"GetRatesLocation7 - ${Thread.currentThread().getName}")
    10
  }(executor.pool3)

  def getRatesLocation8: ToyFuture[Int] = ToyFuture{
    Thread.sleep(2000)
    println(s"GetRatesLocation8 - ${Thread.currentThread().getName}")
    20
  }(executor.pool3)


}










object executor {
      val pool1: ExecutorService =
        Executors.newFixedThreadPool(2, NameableThreads("fixed-pool-1"))
      val pool2: ExecutorService =
        Executors.newCachedThreadPool(NameableThreads("cached-pool-2"))
      val pool3: ExecutorService =
        Executors.newWorkStealingPool(4)
      val pool4: ExecutorService =
        Executors.newSingleThreadExecutor(NameableThreads("singleThread-pool-4"))
}