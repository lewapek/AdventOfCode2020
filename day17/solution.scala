import scala.io.Source
import scala.annotation.tailrec

val input = Source.fromFile("input").getLines.foldLeft(Array.empty[Array[Char]])(_ appended _.toArray)
val s = State(Array(input), 1, input.length, input.head.length)

val cycles = 6
val Active = '#'
val Inactive = '.'

case class State(v: Array[Array[Array[Char]]], d: Int, h: Int, w: Int) {
  def makeBorder: State = {
    val updatedCurrentLayer =
      v.map { y =>
        Array.fill(w + 2)(Inactive) +:
          y.map(row => Inactive +: row :+ Inactive) :+
          Array.fill(w + 2)(Inactive)
      }
    val newV = 
      Array.fill(h + 2)(Array.fill(w + 2)(Inactive)) +:
        updatedCurrentLayer :+
        Array.fill(h + 2)(Array.fill(w + 2)(Inactive))
    State(newV, d + 2, h + 2, w + 2)
  }
}

def neighbours(z: Int, y: Int, x: Int, d: Int, h: Int, w: Int): List[(Int, Int, Int)] =
  for {
    c <- List(z - 1, z, z + 1)
    if c >= 0 && c < d
    b <- List(y - 1, y, y + 1)
    if b >= 0 && b < h
    a <- List(x - 1, x, x + 1)
    if a >= 0 && a < w
    if (c, b, a) != (z, y, x)
  } yield (c, b, a)

def activeNeighbours(v: Array[Array[Array[Char]]], z: Int, y: Int, x: Int, d: Int, h: Int, w: Int): Int =
  neighbours(z, y, x, d, h, w).count { case (zn, yn, xn) =>
    //println(z, y, x, zn, yn, xn, v.size, v(0).size, v(0)(0).size, d, h, w)
    v(zn)(yn)(xn) == Active
  }

def transform(s: State): State = {
  val newArr = Array.fill(s.d)(Array.fill(s.h)(Array.fill(s.w)(Inactive)))
  for {
    z <- 0 to (s.d - 1)
    y <- 0 to (s.h - 1)
    x <- 0 to (s.w - 1)
  } yield {
    val ans = activeNeighbours(s.v, z, y, x, s.d, s.h, s.w)
    s.v(z)(y)(x) match {
      case Active if ans == 2 || ans == 3 => newArr(z)(y)(x) = Active
      case Active => newArr(z)(y)(x) = Inactive
      case Inactive if ans == 3 => newArr(z)(y)(x) = Active
      case Inactive => newArr(z)(y)(x) = Inactive
      case other => newArr(z)(y)(x) = other
    }
  }

  s.copy(v = newArr).makeBorder
}

@tailrec
def iter(s: State, n: Int, i: Int = 0): State = {
  if (i == n) s
  else {
    iter(transform(s), n, i + 1)
  }
}

val finalState = iter(s.makeBorder, 6, 0)

def print(s: State): Unit = {
  s.v.foreach { d2 =>
    println("---")
    d2.foreach(d1 => println(d1.mkString))
  }
  println()
}

//println(input.toList.map(_.toList))
//print(finalState)
val activeCount = finalState.v.map(_.map(_.count(_ == Active)).sum).sum
println(activeCount)


