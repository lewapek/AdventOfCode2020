import scala.io.Source
import scala.annotation.tailrec

val filename = "input"
val lines = Source.fromFile(filename).getLines
val leave = lines.next().toInt
val departures = lines.next().split(',').foldLeft(List.empty[Int]) { (acc, line) =>
  if (line == "x") acc else line.toInt :: acc
}

val diffsMap = departures.map { d =>
  val diff = (d - leave % d) % d
  diff -> d
}.toMap

val (minDiff, id) = diffsMap.min
println(minDiff, id, minDiff * id)

val (offsets, _) =
  Source.fromFile(filename).getLines.toVector.last.split(',')
    .foldLeft((Vector.empty[(Long, Long)], 0L)) { case ((acc, offset), line) =>
      if (line == "x") (acc, offset + 1)
      else (acc :+ (line.toLong, offset), offset + 1)
    }
val sortedAN = offsets.map { case (ni, ai) => ((ni - ai % ni) % ni, ni) }.sortBy(-_._2)

@tailrec
def fs(startA: Long, startN: Long, targetA: Long, targetN: Long): Long = {
  if (startA % targetN == targetA) startA
  else fs(startA + startN, startN, targetA, targetN)
}

@tailrec
def find(a: Long, n: Long)(nas: Vector[(Long, Long)]): Long = {
  if (nas.isEmpty) a
  else {
    val (targetA, targetN) = nas.head
    val newA = fs(a, n, targetA, targetN)
    val newN = n * targetN
    find(newA, newN)(nas.tail)
  }
}

println(find(sortedAN.head._1, sortedAN.head._2)(sortedAN.tail))
