import scala.io.Source

case class Input(groups: List[List[String]] = List.empty, startNew: Boolean = true)

val input = Source.fromFile("input").getLines.foldLeft(Input()) { (acc, line) =>
  if (line.isEmpty) {
    acc.copy(startNew = true)
  } else {
    val groups = acc.groups
    val newGroups = 
      if (acc.startNew) List(line) :: groups
      else (line :: groups.head) :: groups.tail
    Input(newGroups, false)
  }
}

val groups = input.groups

def coutntDistincsAnswers(xs: List[String]): Int =
  xs.flatMap(_.toList).toSet.size

val sumOfDistinct = groups.map(coutntDistincsAnswers).sum
println(sumOfDistinct)

def countSameAnswers(xs: List[String]): Int =
  xs.tail.foldLeft(xs.head.toSet)(_ intersect _.toSet).size

val sumOfSame = groups.map(countSameAnswers).sum
println(sumOfSame)

