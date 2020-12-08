import scala.io.Source
import scala.annotation.tailrec

sealed abstract class Instruction {
  val v: Int
}
case class Acc(v: Int) extends Instruction
case class Jmp(v: Int) extends Instruction
case class Nop(v: Int) extends Instruction

val instructions = Source.fromFile("input").getLines.map { line =>
  val Array(i, v) = line.split(' ').map(_.trim)
  i match {
    case "acc" => Acc(v.toInt)
    case "jmp" => Jmp(v.toInt)
    case "nop" => Nop(v.toInt)
  }
}.toVector

@tailrec
def runUntilLoop(xs: Vector[Instruction], i: Int = 0, visited: Set[Int] = Set.empty, acc: Int = 0): Int = {
  if (visited contains i) acc
  else {
    xs(i) match {
      case Acc(v) => runUntilLoop(xs, i + 1, visited + i, acc + v)
      case Jmp(v) => runUntilLoop(xs, i + v, visited + i, acc)
      case Nop(_) => runUntilLoop(xs, i + 1, visited + i, acc)
    }
  }
}

println(runUntilLoop(instructions))

@tailrec
def runIfTerminates(xs: Vector[Instruction], i: Int = 0, visited: Set[Int] = Set.empty, acc: Int = 0): Option[Int] = {
  if (visited contains i) None
  else if (i == xs.size) Some(acc)
  else if (i > xs.size) None
  else {
    xs(i) match {
      case Acc(v) => runIfTerminates(xs, i + 1, visited + i, acc + v)
      case Jmp(v) => runIfTerminates(xs, i + v, visited + i, acc)
      case Nop(_) => runIfTerminates(xs, i + 1, visited + i, acc)
    }
  }
}

def runChanged(xs: Vector[Instruction]): Option[Int] = {
  (0 until xs.size).foldLeft(None: Option[Int]) { (acc, i) =>
    acc match {
      case s: Some[Int] => s
      case None =>
        xs(i) match {
          case Acc(_) =>
            acc
          case Jmp(v) =>
            val (a, b) = xs.splitAt(i)
            runIfTerminates(a ++ Vector(Nop(v)) ++ b.tail)
          case Nop(v) =>
            val (a, b) = xs.splitAt(i)
            runIfTerminates(a ++ Vector(Jmp(v)) ++ b.tail)
        }
    }
  }
}

println(runChanged(instructions))

