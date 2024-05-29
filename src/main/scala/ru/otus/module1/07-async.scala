package ru.otus.module1


import ru.otus.module1.utils.NameableThreads

import java.io.File
import java.util.concurrent.{Executor, ExecutorService, Executors}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.global
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future, Promise}
import scala.io.{BufferedSource, Source}
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try}

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

object try_{

  def readFromFile(): List[String] = {
    val s: BufferedSource = Source.fromFile(new File("ints.txt"))
    val result = try{
      s.getLines().toList
    } catch {
      case e: Exception =>
        println(e.getMessage)
        Nil
    } finally{
      s.close()
    }
    result
  }

  def readFromFile2(): Try[List[String]] = {
    val s: BufferedSource = Source.fromFile(new File("ints.txt"))
    val result = Try(s.getLines().toList)
    s.close()
    result
  }

  readFromFile2().foreach(l => l.foreach(println))
  readFromFile2().recover{
    case e: Throwable =>
      println(e.getMessage)
      List("")
  }


  def readFromFile3(): Try[List[String]] = {
    val s: Try[BufferedSource] = Try(Source.fromFile(new File("ints.txt")))
    def lines(s: Source) = Try(s.getLines().toList)
    val result = for{
      source <- s
      l <- lines(source)
    } yield "foo" :: l

    val r2: Try[List[String]] = s.flatMap(src => lines(src))
    s.foreach(_.close())
    result
  }

}




object future{
  // constructors

  val f1: Future[Int] = Future.successful(10)
  val f2: Future[Int] = Future.failed[Int](new Throwable("ooops"))
  val f3: Future[Int] = Future(10 + 10)(scala.concurrent.ExecutionContext.global)



  // combinators
  def longRunningComputation: Int = ???

  f3.map( _ + 10)(scala.concurrent.ExecutionContext.global)
  f3.flatMap(i => Future.successful(i + 10))(scala.concurrent.ExecutionContext.global)

  f2.recover{
    case e: Exception => 0
  }(scala.concurrent.ExecutionContext.global)

  f2.onComplete{
    case Success(value) =>
      println(value)
    case Failure(exception) =>
     println(exception.getMessage)
  }(scala.concurrent.ExecutionContext.global)

  def future1: Future[Int] = ???
  def future2: Future[Int] = ???


  def getRatesLocation1: Future[Int] = Future{
    Thread.sleep(1000)
    println(s"GetRatesLocation1 - ${Thread.currentThread().getName}")
    10
  }(scala.concurrent.ExecutionContext.global)

  def getRatesLocation2: Future[Int] = Future{
    Thread.sleep(2000)
    println(s"GetRatesLocation2 - ${Thread.currentThread().getName}")
    20
  }(scala.concurrent.ExecutionContext.global)


//  def printRunningTime(f: => Unit) = {
//    val start = System.currentTimeMillis()
//    f
//    val end = System.currentTimeMillis()
//    println(s"Running time: ${end - start}")
//  }

  import scala.concurrent.ExecutionContext.Implicits.global

  def printRunningTime[T](f: => Future[T]): Future[T] = for{
    start <- Future.successful(System.currentTimeMillis())
     t <- f
    end <- Future.successful(System.currentTimeMillis())
    _ = println(s"Running time: ${end - start}")
  } yield t




  def action(v: Int): Int = {
    Thread.sleep(1000)
    println(s"Action $v in ${Thread.currentThread().getName}")
    v
  }


  // Execution contexts

  val ec1: ExecutionContext = ExecutionContext.fromExecutor(executor.pool1)
  val ec2: ExecutionContext = ExecutionContext.fromExecutor(executor.pool2)
  val ec3: ExecutionContext = ExecutionContext.fromExecutor(executor.pool3)
  val ec4: ExecutionContext = ExecutionContext.fromExecutor(executor.pool4)

  val f01= Future(action(10))(ec1)
  val f02= Future(action(20))(ec2)

  val f03 = f01.flatMap{ v1 =>
    action(50)
    f02.map{ v2 =>
      action(v1 + v2)
    }(ec4)
  }(ec3)



}

object promise {

 def flatMap[T, B](f1: Future[T])(f: T => Future[B])(implicit ec: ExecutionContext): Future[B] = {

   val p = Promise[B]
   f1.onComplete{
     case Success(value) =>
       f(value).onComplete {
           v => p.complete(v)
       }
     case Failure(exception) =>
       p.failure(exception)

   }
   p.future
 }


   val p1: Promise[Int] = Promise[Int] // isCompleted false
   val f1: Future[Int] = p1.future // isCompleted false
   p1.complete(Try(10))
   p1.isCompleted // true
   f1.isCompleted // true
}