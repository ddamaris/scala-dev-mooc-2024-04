import MyMath.Logarithm

object homework1 {
    @main def part1Ex(): Unit ={
      extension (x: String)
        def +(y: String): String = x.concat(y)
      val res = "1" + "33"
      println(res)
    }
}

object homework2 {
  enum CompletionArg {
    private case ShowItIsString(s: String)
    private case ShowItIsInt(i: Int)
    private case ShowItIsFloat(f: Float)
  }

  private object CompletionArg {
    given Conversion[String, CompletionArg] = ShowItIsString(_)
    given Conversion[Int, CompletionArg] = ShowItIsInt(_)
    given Conversion[Float, CompletionArg] = ShowItIsFloat(_)

    def complete[T](arg: CompletionArg): Any = arg match
      case ShowItIsString(s) => s
      case ShowItIsInt(i) => i
      case ShowItIsFloat(f) => f
  }

  import CompletionArg.*

  @main def part2Ex(): Unit ={
    println(CompletionArg.complete("String"))
    println(CompletionArg.complete(1))
    println(CompletionArg.complete(7f))
  }
}

object homework3 {
  opaque type Logarithm = Double

  object Logarithm {
    def apply(d: Double): Logarithm = math.log(d)

    extension (x: Logarithm)
      def toDouble: Double = math.exp(x)
      def +(y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
      def *(y: Logarithm): Logarithm = x + y
  }
  
  @main def part3Ex(): Unit ={
    import homework3.Logarithm

    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l2
    println(l4)

  }
}