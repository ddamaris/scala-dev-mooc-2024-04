import monad.Wrap.empty

package object monad {

  /**
   * Реализуйте методы map / flatMap / withFilter чтобы работал код и законы монад соблюдались
   * HINT: для проверки на пустой элемент можно использовать eq
   */

  trait Wrap[+A] {

    def get: A

    def pure[R](x: R): Wrap[R] = NonEmptyWrap[R](x)

    def flatMap[R](f: A => Wrap[R]): Wrap[R] =
      this match {
        case NonEmptyWrap(v) => f(v)
        case EmptyWrap => empty[R]
    }

    // HINT: map можно реализовать через pure и flatMap
    def map[R](f: A => R): Wrap[R] = {
      flatMap(x => pure(f(x)))
    }

    def withFilter(f: A => Boolean): Wrap[A] = this match {
      case w@NonEmptyWrap(x: A) if f(x) => w
      case _ => empty[A]
    }

  }

  object Wrap {
    def empty[R]: Wrap[R] = EmptyWrap
  }

  case class NonEmptyWrap[A](result: A) extends Wrap[A] {
    override def get: A = result
  } // pure

  case object EmptyWrap extends Wrap[Nothing] {
    override def get: Nothing = throw new NoSuchElementException("Wrap.get")
  } // bottom, null element

}