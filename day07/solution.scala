import scala.io.Source

val input =
  Source.fromFile("input").getLines
    .foldLeft(Map.empty[String, Map[String, Int]]) { (acc, line) =>
      val Array(keyBag, value) = line.split("contain").map(_.trim)
      val key = keyBag.dropRight(5)
      if (value.startsWith("no")) acc + (key -> Map.empty)
      else {
        val valuesMap = value.dropRight(1).split(',').map(_.trim)
          .foldLeft(Map.empty[String, Int]) { (innerAcc, bag) =>
            val Array(n, nameA, nameB, _) = bag.split(' ')
            innerAcc + ((nameA + " " + nameB) -> n.toInt)
          }
        acc + (key -> valuesMap)
      }
    }

def count(name: String)(xs: Map[String, Map[String, Int]]): (Int, Map[String, Map[String, Int]]) = {
  val (found, other) = xs.partition { case (_, v) => v contains name }
  val n = found.size
  if (n > 0) {
    found.keySet.foldLeft((n, other)) { case ((accSum, accM), key) =>
      val (i, m) = count(key)(accM)
      (i + accSum, m)
    }
  } else (0, other)
}

println(count("shiny gold")(input)._1)

def countBags(name: String)(xs: Map[String, Map[String, Int]]): Int = {
  xs(name).map { case (k, v) => v + v * countBags(k)(xs) }.sum
}

println(countBags("shiny gold")(input))

