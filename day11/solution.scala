import scala.io.Source
import scala.concurrent.duration.Duration
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.annotation.tailrec

object Main {

  type Layout = Array[Array[Char]]

  val seats = Source.fromFile("input").getLines.foldLeft(Array.empty[Array[Char]])(_ appended _.toArray)
  
  val Empty = 'L'
  val Occupied = '#'
  val Floor = '.'
  
  val h = seats.length
  val w = seats(0).length
  
  def neighbours(i: Int, j: Int, w: Int, h: Int): Set[(Int, Int)] = {
    for {
      ni <- Set(i - 1, i, i + 1)
      if ni >= 0 && ni < w
      nj <- Set(j - 1, j, j + 1)
      if nj >= 0 && nj < h
      if (ni, nj) != (i, j)
    } yield (ni, nj)
  }
  
  def noOccupiedNeighbours(xs: Layout, w: Int, h: Int, i: Int, j: Int): Boolean =
    neighbours(i, j, w, h).forall { case (a, b) => xs(b)(a) != Occupied }
  
  def atLeastNOccupiedNeighbours(n: Int)(xs: Layout, w: Int, h: Int, i: Int, j: Int): Boolean =
    neighbours(i, j, w, h).count { case (a, b) => xs(b)(a) == Occupied } >= n
  
  var i = 0
  def makeIteration(xs: Layout, w: Int, h: Int): Layout = {
    val res = Array.fill(h)(Array.fill(w)(Floor))
    val changes = (0 until w).toList.map { i =>
      (0 until h).toList.map { j =>
        Future {
          xs(j)(i) match {
            case Empty if noOccupiedNeighbours(xs, w, h, i, j) => res(j)(i) = Occupied
            case Occupied if atLeastNOccupiedNeighbours(4)(xs, w, h, i, j) => res(j)(i) = Empty
            case other => res(j)(i) = other
          }
        }
      }
    }
    Await.result(Future.sequence(changes.flatten), Duration.Inf)
    i += 1
    //println(i, res.map(_.count(_ == Occupied)).sum, res.map(_.count(_ == Empty)).sum)
    res
  }
  
  def areIdentical(xs: Layout, ys: Layout, h: Int): Boolean =
    (0 until h).forall(i => xs(i) sameElements ys(i))
  
  @tailrec
  def iterateUntilStable(xs: Layout, w: Int, h: Int): Layout = {
    val ys = makeIteration(xs, w, h)
    if (areIdentical(xs, ys, h)) ys
    else iterateUntilStable(ys, w, h)
  }
  
  def run(): Unit = {
    val stable = iterateUntilStable(seats, w, h)
    val occupiedCount = stable.map(_.count(_ == Occupied)).sum
    println(occupiedCount)
  }

  val directions: List[(Int, Int)] =
    for {
      dx <- List(-1, 0, 1)
      dy <- List(-1, 0, 1)
      if (dx, dy) != (0, 0)
    } yield (dx, dy)

  @tailrec
  def isOccupiedInDirection(xs: Layout, w: Int, h: Int, i: Int, j: Int, dx: Int, dy: Int): Boolean = {
    val (newI, newJ) = (i + dx, j + dy)
    if (newI < 0 || newI >= w || newJ < 0 || newJ >= h) false
    else {
      val e = xs(newJ)(newI)
      if (e == Occupied) true
      else if (e == Empty) false
      else isOccupiedInDirection(xs, w, h, newI, newJ, dx, dy)
    }
  }

  def noOccupiedAllDirections(xs: Layout, w: Int, h: Int, i: Int, j: Int): Boolean =
    directions.forall { case (dx, dy) => !isOccupiedInDirection(xs, w, h, i, j, dx, dy) }

  def atLeastNOccupiedAllDirections(n: Int)(xs: Layout, w: Int, h: Int, i: Int, j: Int): Boolean =
    directions.count { case(dx, dy) => isOccupiedInDirection(xs, w, h, i, j, dx, dy) } >= n

  def makeIteration2(xs: Layout, w: Int, h: Int): Layout = {
    val res = Array.fill(h)(Array.fill(w)(Floor))
    val changes = (0 until w).toList.map { i =>
      (0 until h).toList.map { j =>
        Future {
          xs(j)(i) match {
            case Empty if noOccupiedAllDirections(xs, w, h, i, j) => res(j)(i) = Occupied
            case Occupied if atLeastNOccupiedAllDirections(5)(xs, w, h, i, j) => res(j)(i) = Empty
            case other => res(j)(i) = other
          }
        }
      }
    }
    Await.result(Future.sequence(changes.flatten), Duration.Inf)
    res
  }

  @tailrec
  def iterateUntilStable2(xs: Layout, w: Int, h: Int): Layout = {
    val ys = makeIteration2(xs, w, h)
    if (areIdentical(xs, ys, h)) ys
    else iterateUntilStable2(ys, w, h)
  }

  def run2(): Unit = {
    val stable = iterateUntilStable2(seats, w, h)
    val occupiedCount = stable.map(_.count(_ == Occupied)).sum
    println(occupiedCount)
  }

}

Main.run()
Main.run2()

