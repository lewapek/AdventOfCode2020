import scala.io.Source
import scala.annotation.tailrec

val input = Source.fromFile("input").getLines.foldLeft(Array.empty[Array[Char]])(_ appended _.toArray)
val s = State(Array(Array(input)), 1, 1, input.length, input.head.length)

val Active = '#'
val Inactive = '.'
type A4d = Array[Array[Array[Array[Char]]]]
type A3d = Array[Array[Array[Char]]]

case class State(v: A4d, d4: Int, d: Int, h: Int, w: Int) {
  def makeBorder: State = {
    val array4d =
      Array.fill(d + 2)(Array.fill(h + 2)(Array.fill(w + 2)(Inactive))) +:
        v.map(e => make3dBorder(e)) :+
        Array.fill(d + 2)(Array.fill(h + 2)(Array.fill(w + 2)(Inactive)))

    State(array4d, d4 + 2, d + 2, h + 2, w + 2)
  }

  private def make3dBorder(a3d: A3d): A3d = {
    val updatedCurrentLayer =
      a3d.map { y =>
        Array.fill(w + 2)(Inactive) +:
          y.map(row => Inactive +: row :+ Inactive) :+
          Array.fill(w + 2)(Inactive)
      }
    val newV =
      Array.fill(h + 2)(Array.fill(w + 2)(Inactive)) +:
        updatedCurrentLayer :+
        Array.fill(h + 2)(Array.fill(w + 2)(Inactive))
    newV
  }
}

def neighbours(z4: Int, z: Int, y: Int, x: Int, d4: Int, d: Int, h: Int, w: Int): List[(Int, Int, Int, Int)] =
  for {
    c4 <- List(z4 - 1, z4, z4 + 1)
    if c4 >= 0 && c4 < d4
    c <- List(z - 1, z, z + 1)
    if c >= 0 && c < d
    b <- List(y - 1, y, y + 1)
    if b >= 0 && b < h
    a <- List(x - 1, x, x + 1)
    if a >= 0 && a < w
    if (c4, c, b, a) != (z4, z, y, x)
  } yield (c4, c, b, a)

def activeNeighbours(v: A4d, z4: Int, z: Int, y: Int, x: Int, d4: Int, d: Int, h: Int, w: Int): Int =
  neighbours(z4, z, y, x, d4, d, h, w).count { case (z4n, zn, yn, xn) =>
    v(z4n)(zn)(yn)(xn) == Active
  }

def transform(s: State): State = {
  val newArr = Array.fill(s.d4)(Array.fill(s.d)(Array.fill(s.h)(Array.fill(s.w)(Inactive))))
  for {
    z4 <- 0 until s.d4
    z <- 0 until s.d
    y <- 0 until s.h
    x <- 0 until s.w
  } yield {
    val ans = activeNeighbours(s.v, z4, z, y, x, s.d4, s.d, s.h, s.w)
    s.v(z4)(z)(y)(x) match {
      case Active if ans == 2 || ans == 3 => newArr(z4)(z)(y)(x) = Active
      case Active => newArr(z4)(z)(y)(x) = Inactive
      case Inactive if ans == 3 => newArr(z4)(z)(y)(x) = Active
      case Inactive => newArr(z4)(z)(y)(x) = Inactive
      case other => newArr(z4)(z)(y)(x) = other
    }
  }

  s.copy(v = newArr).makeBorder
}

@tailrec
def iter(s: State, n: Int, i: Int = 0): State = {
  println(s"i = $i")
  if (i == n) s
  else {
    iter(transform(s), n, i + 1)
  }
}

val finalState = iter(s.makeBorder, 6, 0)

//print(finalState)
val activeCount = finalState.v.map(_.map(_.map(_.count(_ == Active)).sum).sum).sum
println(activeCount)

