import scala.io.Source
import scala.annotation.tailrec

val input = Source.fromFile("input").getLines.map { line =>
  (line.substring(0, 7), line.substring(7, 10))
}

@tailrec
def asBinaryInt(low: Char, high: Char)(s: String, m: Int = 1, acc: Int = 0): Int = {
  s.lastOption match {
    case None => acc
    case Some(c) =>
      val ith = if (c == low) 0 else m
      asBinaryInt(low, high)(s.dropRight(1), m << 1, acc | ith)
  }
}

val set = input.map { case (a, b) =>
  (asBinaryInt('F', 'B')(a) << 3) | asBinaryInt('L', 'R')(b)
}.toSet

println(set.max)
val missing = Range(set.min, set.max).toSet diff set
println(missing)

