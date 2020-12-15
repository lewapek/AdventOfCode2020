import scala.io.Source
import scala.annotation.tailrec

val ns = Source.fromFile("input").getLines.next.split(',').map(_.toInt).toList.zipWithIndex.toMap

println(ns)

@tailrec
def iter(n: Int, i: Int, max: Int, ns: Map[Int, Int]): Int = {
  if (i == max) n
  else {
    if (i % 1000000 == 0) println(s"i = $i")
    if (ns contains n) {
      val newN = i - ns(n)
      iter(newN, i + 1, max, ns + (n -> i))
    }
    else {
      iter(0, i + 1, max, ns + (n -> i))
    }
  }
}

val res1 = iter(0, ns.size, 2019, ns)
println(res1)

val max = 30000000 - 1
val res2 = iter(0, ns.size, max, ns)
println(res2)

