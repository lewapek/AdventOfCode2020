import scala.io.Source

sealed trait Elem
case class Mask(and: Long, or: Long, floating: List[Int] = List.empty) extends Elem
case class Assignment(address: Long, value: Long) extends Elem

val elems = Source.fromFile("input").getLines.map { line =>
  if (line.startsWith("mask")) {
    val Array(_, str) = line.split('=').map(_.trim)
    val (and, or, floating, _) = str.foldLeft((0L, 0L, List.empty[Int], 35)) { case ((and, or, f, i), char) =>
      val m = 1L << i
      if (char == 'X') (and | m, or, i :: f, i - 1)
      else if (char == '1') (and | m, or | m, f, i - 1)
      else (and, or, f, i - 1)
    }
    Mask(and, or, floating)
  }
  else {
    val Array(strA, strV) = line.split('=').map(_.trim)
    Assignment(strA.drop(4).dropRight(1).toLong, strV.toLong)
  }
}.toList

val (addresses, _) = elems.foldLeft((Map.empty[Long, Long]), Mask(0, 0)) { case ((map, mask), e) =>
  e match {
    case m: Mask => (map, m)
    case Assignment(a, v) => (map + (a -> ((v | mask.or) & mask.and)), mask)
  }
}

println(addresses.values.sum)

def genAddresses(a: Long, floating: List[Int]): List[Long] = floating match {
  case i :: tail => genAddresses(a | (1L << i), tail) ::: genAddresses(a & ~(1L << i), tail)
  case Nil => List(a)
}

val (addressesFloating, _) = elems.foldLeft((Map.empty[Long, Long]), Mask(0, 0)) { case ((map, mask), e) =>
  e match {
    case m: Mask => (map, m)
    case Assignment(a, v) =>
      val baseA = a | mask.or
      val newMap = genAddresses(baseA, mask.floating).foldLeft(map) { (acc, addr) =>
        acc + (addr -> v)
      }
      (newMap, mask)
  }
}
println(addressesFloating.values.sum)

