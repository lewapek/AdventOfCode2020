import scala.io.Source
import scala.collection.mutable

val input = Source.fromFile("input").getLines.map(_.toInt).toArray.sorted
val max = input.last
val diff = 3
val adapters = 0 +: input :+ (max + diff)

val diffs = 
  (0 until (adapters.length - 1))
    .foldLeft(Array.fill(diff)(0)) { (arr, i) =>
      val d = adapters(i + 1) - adapters(i)
      arr(d - 1) += 1
      arr
    }

val resPart1 = diffs.head * diffs.last
println(resPart1)

def ways(xs: Array[Int], diff: Int): Long = {
  val len = xs.length
  val cache = mutable.Map.empty[Int, Long]

  def dp(index: Int): Long = {
    if (index == len - 1) 1
    else if (cache contains index) cache(index)
    else {
      cache(index) =
        ((index + 1) until len).map { i =>
          if (xs(i) - xs(index) <= diff) dp(i) else 0L
        }.sum
      cache(index)
    }
  }

  dp(0)
}

println(ways(adapters, 3))

