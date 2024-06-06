package ru.otus.module2

import ru.otus.module2.type_classes.JsValue.{JsNull, JsNumber, JsString}


object type_classes {

  sealed trait JsValue
  object JsValue {
    final case class JsObject(get: Map[String, JsValue]) extends JsValue
    final case class JsString(get: String) extends JsValue
    final case class JsNumber(get: Double) extends JsValue
    final case object JsNull extends JsValue
  }

  // 1
  trait JsonWriter[T]{
    def write(v: T): JsValue
  }



  object JsonWriter{
    // summoner
    def apply[T](implicit ev: JsonWriter[T]): JsonWriter[T] = ev

    def from[T](f: T => JsValue) = new JsonWriter[T] {
      override def write(v: T): JsValue = f(v)
    }

    implicit val strJson = from[String](JsString)

    implicit val intJson = from[Int](JsNumber(_))

    implicit def optToJson[T](implicit ev: JsonWriter[T]) = from[Option[T]] {
      case Some(value) => ev.write(value)
      case None => JsNull
    }
  }
  // 3
  def toJson[T : JsonWriter](v: T): JsValue = {
    JsonWriter[T].write(v)
  }

  // 4
  implicit class JsonSyntax[T](v: T){
    def toJson(implicit ev: JsonWriter[T]): JsValue = ev.write(v)
  }

  "vfbfbgfbg".toJson
  10.toJson
  toJson("bgfbghnbggng")
  toJson(12)
  toJson(Option(12))
  toJson(Option("vffbfgg"))

  // 1 компонент
  trait Ordering[T]{
    def less(a: T, b: T): Boolean
  }

  object Ordering{

    def from[A](f: (A, A) => Boolean): Ordering[A] = new Ordering[A]{
      override def less(a: A, b: A): Boolean = f(a, b)
    }
    // 2
    implicit val intOrdering = from[Int](_ < _)

    implicit val strOrdering =  from[String](_ < _)
  }


  // 3
  def greatest[A](a: A, b: A)(implicit ordering: Ordering[A]): A =
    if(ordering.less(a, b)) b else a

  greatest(5, 10) // 10
  greatest("ab", "cbd") // "cbd"

  // 2 пример

  // 1
  trait Eq[T]{
    def ===(a: T, b: T): Boolean
  }

  object Eq{

    def ===[T](a: T, b: T)(implicit eq: Eq[T]) = eq.===(a, b)

    implicit val strEq = new Eq[String]{
      override def ===(a: String, b: String): Boolean = a == b
    }
  }



  implicit class EqSyntax[T](a: T){
    // 3
    def ===(b: T)(implicit eq: Eq[T]): Boolean = eq.===(a, b)
  }


  val result = List("a", "b", "c").filter(str => str === "")










}
