import scala.io.Source
import scala.annotation.tailrec

case class RangeOr(rs: Set[Range.Inclusive]) {
  def contains(n: Int): Boolean =
    rs.exists(_ contains n)
}

case class Constraints(map: Map[String, RangeOr]) {
  def contains(n: Int): Boolean =
    map.values.exists(_ contains n)
}

val lines = Source.fromFile("input").getLines

val constraintsMap = lines.takeWhile(_.nonEmpty).map { line =>
  val Array(name, c) = line.split(':').map(_.trim)
  val elements = c.split("or").map(_.trim).map { range =>
    val Array(min, max) = range.split('-').map(_.toInt)
    min to max
  }.toSet
  name -> RangeOr(elements)
}.toList.toMap
val cs = Constraints(constraintsMap)

val myTicket: List[Int] = lines.takeWhile(_.nonEmpty).toList.last.split(',').map(_.toInt).toList
lines.next()
val tickets: List[List[Int]] = lines.map(_.split(',').map(_.toInt).toList).toList

//println(constraints)
//println(myTicket)
//tickets.foreach(println)

val errorRate =
  tickets.flatMap { t =>
    t.filterNot(cs.contains)
  }.sum
println(errorRate)

val validTickets = tickets.filter { t =>
  t.forall(cs.contains)
}
val ticketsCols = validTickets.transpose
//println(ticketsCols)

def findPossible(ns: List[Int], cs: Constraints): Set[String] = {
  cs.map.filter { case (k, rangeOr) => ns.forall(rangeOr.contains) }.keys.toSet
}

val possibleColumns = ticketsCols.map(c => findPossible(c, cs)).zipWithIndex.map { case (k, v) => (v, k) }.toMap
//println(possibleColumns)

@tailrec
def findColumns(possible: Map[Int, Set[String]], res: Map[Int, String] = Map.empty): Map[Int, String] = {
  val remainingCount = possible.count { case (_, v) => v.size > 1 }
  if (remainingCount == 0) res
  else {
    val singles = possible.filter { case (_, v) => v.size == 1 }.map { case (k, v) => k -> v.head }
    val foundNames = singles.values.toSet
    val newPossible = possible.map { case (k, v) => k -> (v -- foundNames) }
    findColumns(newPossible, res ++ singles)
  }
}

val found = findColumns(possibleColumns)
//println(found)

val indexes = found.filter { case (_, v) => v.startsWith("departure") }.keys
val mul = indexes.map(i => myTicket(i).toLong).reduce(_ * _)
println(mul)

