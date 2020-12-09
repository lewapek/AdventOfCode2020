import scala.io.Source

type N = Long

val ns = Source.fromFile("input").getLines.map(_.toLong).toArray

def sum2(ns: Array[N], a: Int, b: Int, target: N): Boolean = {
  val (_, res) = (a to b).foldLeft((Set.empty[N], false)) { case ((visited, found), i) =>
    if (found) (visited, found)
    else {
      val remaining = target - ns(i)
      if (visited contains remaining) (visited, true)
      else (visited + ns(i), false)
    }
  }
  res
}

def findInvalid(ns: Array[N], a: Int, b: Int): Option[N] = {
  if (b >= ns.length) None
  else {
    val target = ns(b)
    val s2 = sum2(ns, a, b - 1, target)
    if (s2) findInvalid(ns, a + 1, b + 1)
    else Some(target)
  }
}

val invalid = findInvalid(ns, 0, 25)
println(invalid)


def findContiguousSummingTo(n: N)(ns: Array[N]): Option[(Int, Int)] = {
  ((ns.size - 1) to 1 by -1).foldLeft(None: Option[(Int, Int)]) { (acc, si) =>
    acc match {
      case some: Some[(Int, Int)] => acc
      case None =>
        val end = si - 1
        val (s, f, i) = (end to 0 by -1)
          .foldLeft((ns(si), false, -1)) { case ((sum, found, foundIndex), ei) =>
            if (found) (sum, found, foundIndex)
            else {
              val newSum = sum + ns(ei)
              if (newSum == n) (newSum, true, ei)
              else (newSum, false, foundIndex)
            }
          }
        if (f) Some((si, i))
        else None
    }
  }
}

val Some((endIndex, startIndex)) = findContiguousSummingTo(invalid.get)(ns)
val slice = ns.slice(startIndex, endIndex + 1)
val res = slice.min + slice.max
println(res)

