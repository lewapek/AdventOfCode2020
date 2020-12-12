import scala.io.Source

sealed trait Instr {
  val v: Int
}
case class N(v: Int) extends Instr
case class S(v: Int) extends Instr
case class E(v: Int) extends Instr
case class W(v: Int) extends Instr
case class L(v: Int) extends Instr
case class R(v: Int) extends Instr {
  def asL: L = L((4 - v) % 4)
}
case class F(v: Int) extends Instr

case class Pos(x: Int, y: Int, dx: Int, dy: Int)

val instructions = Source.fromFile("input").getLines.map(_.splitAt(1)).map {
  case ("N", n) => N(n.toInt)
  case ("S", n) => S(n.toInt)
  case ("E", n) => E(n.toInt)
  case ("W", n) => W(n.toInt)
  case ("L", n) => L((n.toInt / 90) % 4)
  case ("R", n) => R((n.toInt / 90) % 4).asL
  case ("F", n) => F(n.toInt)
}.toList

def move(i: Instr)(p: Pos): Pos = i match {
  case F(v) => p.copy(x = p.x + v * p.dx, y = p.y + v * p.dy)
  case N(v) => p.copy(y = p.y + v)
  case S(v) => p.copy(y = p.y - v)
  case E(v) => p.copy(x = p.x + v)
  case W(v) => p.copy(x = p.x - v)
  case L(v) => 
    val (newDx, newDy) =
      if (v == 0) (p.dx, p.dy)
      else if (v == 1) {
        if (p.dx == 0) (-p.dy, 0)
        else (0, p.dx)
      }
      else if (v == 2) (-p.dx, -p.dy)
      else { // v == 3
        if (p.dx == 0) (p.dy, 0)
        else (0, -p.dx)
      }
    p.copy(dx = newDx, dy = newDy)
}

def follow(instructions: List[Instr])(start: Pos): Pos =
  instructions.foldLeft(start)((p, i) => move(i)(p))

val res1 = follow(instructions)(Pos(0, 0, 1, 0))
println(res1, res1.x.abs + res1.y.abs)

def moveWithWaypoint(i: Instr)(p: Pos): Pos = i match {
  case F(v) => p.copy(x = p.x + v * p.dx, y = p.y + v * p.dy)
  case N(v) => p.copy(dy = p.dy + v)
  case S(v) => p.copy(dy = p.dy - v)
  case E(v) => p.copy(dx = p.dx + v)
  case W(v) => p.copy(dx = p.dx - v)
  case L(v) => 
    val (newDx, newDy) =
      if (v == 0) (p.dx, p.dy)
      else if (v == 1) (-p.dy, p.dx)
      else if (v == 2) (-p.dx, -p.dy)
      else (p.dy, -p.dx)
    p.copy(dx = newDx, dy = newDy)
}

def followWithWaypoint(instructions: List[Instr])(start: Pos): Pos =
  instructions.foldLeft(start) { (p, i) =>
    moveWithWaypoint(i)(p)
  }

val start = Pos(0, 0, 10, 1)
val res2 = followWithWaypoint(instructions)(start)
println(res2, res2.x.abs + res2.y.abs)

