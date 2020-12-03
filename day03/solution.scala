import scala.io.Source
import scala.annotation.tailrec

val input = Source.fromFile("input").getLines.foldLeft(Array.empty[String])(_ appended _)

case class Forest(map: Array[String]) {
  val h = map.length
  val w = map(0).length

  def hasTree(x: Int, y: Int): Boolean = map(y)(x) == '#'
}

case class Pos(x: Int, y: Int, f: Forest) {
  def moveRD(r: Int, d: Int): Option[Pos] = {
    val newX = (x + r) % f.w
    val newY = y + d
    if (newY >= f.h) None else Some(Pos(newX, newY, f))
  }

  def countTree: Int = if (f.hasTree(x, y)) 1 else 0
}

@tailrec
def countTreesOnPath(p: Option[Pos], r: Int, d: Int, acc: Int = 0): Int = p match {
  case None => acc
  case Some(p) => countTreesOnPath(p.moveRD(r, d), r, d, acc + p.countTree)
}

def solutionPart2(init: Pos, slopes: List[(Int, Int)]): Int = {
  slopes.map{ case (r, d) => countTreesOnPath(Some(init), r, d)}.reduce(_ * _)
}

val f = Forest(input)
val initialPos = Pos(0, 0, f)

println(countTreesOnPath(Some(initialPos), 3, 1))
println(solutionPart2(initialPos, List(
  (1, 1),
  (3, 1),
  (5, 1),
  (7, 1),
  (1, 2)
)))

