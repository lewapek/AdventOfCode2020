import scala.io.Source

case class Pass(min: Int, max: Int, c: Char, p: String)

val passwords = Source.fromFile("input").getLines.map { line =>
  val Array(policy, pass) = line.split(':').map(_.trim)
  val Array(minMax, char) = policy.split(' ')
  val Array(min, max) = minMax.split('-')
  Pass(min.toInt, max.toInt, char.head, pass)
}.toList

def countValid(passwords: List[Pass]): Int = {
  passwords.count { p =>
    val quantity = p.p.count(_ == p.c)
    quantity >= p.min && quantity <= p.max
  }
}

val count = countValid(passwords)
println(count)

// part 2
case class Pass2(p1: Int, p2: Int, c: Char, p: String)

val ps2 = passwords.map(p => Pass2(p.min, p.max, p.c, p.p))

def countValid2(ps2: List[Pass2]): Int = {
  ps2.count { p =>
    val a = p.p(p.p1 - 1) == p.c
    val b = p.p(p.p2 - 1) == p.c
    (a && !b) || (!a && b)
  }
}
val count2 = countValid2(ps2)
println(count2)

