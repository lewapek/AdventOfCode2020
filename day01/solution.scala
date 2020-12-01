import scala.io.Source
import scala.collection.mutable

val expenses = Source.fromFile("input").getLines.map(_.toInt).toList

def findSumToN(xs: List[Int], s: Int): Option[(Int, Int)] = {
  val encountered = mutable.Set.empty[Int]
  for {
    x <- xs
  } {
    val complement = s - x
    if (encountered contains complement) {
      return Some((x, complement))
    } else {
      encountered += x
    }
  }
  None
}

def findSum3ToN(xs: List[Int], s: Int): Option[(Int, Int, Int)] = {
  val sorted = xs.sorted.toArray
  val n = xs.length
  var i = 0
  while (i < n - 1) {
    val a = sorted(i)
    var start = i + 1
    var end = n - 1
    while (start < end) {
      if (sorted(start) + sorted(end) + a == s) {
        return Some((a, sorted(start), sorted(end)))
      } else if (sorted(start) + sorted(end) + a < s) {
        start += 1
      } else {
        end -= 1
      }
    }

    i += 1
  }
  None
}

val res = findSumToN(expenses, 2020)
res match {
  case None => println("-")
  case Some((a, b)) => println(s"$a * $b = ${a * b}")
}

val res3 = findSum3ToN(expenses, 2020)
res3 match {
  case None => println("-")
  case Some((a, b, c)) => println(s"$a * $b * $c = ${a * b * c}")
}

