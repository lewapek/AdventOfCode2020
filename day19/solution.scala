import scala.collection.mutable
import scala.io.Source
import scala.concurrent.duration.Duration
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await


object Main {
  val source = Source.fromFile("input")
  val lines = source.getLines()
  val rulesLines = lines.takeWhile(_.nonEmpty)
  
  val rules = rulesLines.map { line =>
    val Array(no, r) = line.split(':').map(_.trim)
    val rs =
      if (r.startsWith("\"")) Set(List(r(1).toString))
      else {
        r.split('|').map(_.trim).foldLeft(Set.empty[List[String]]) { (acc, s) =>
          val list = s.split(' ').map(_.trim).toList
          acc + list
        }
      }
  
    no -> rs
  }.toMap
  
  val msgs = lines.toList
  
  val terminalProductions = Map('a' -> Set("47", "55"), 'b' -> Set("84", "55"))
  val toSubstitude = ("8", "42")
  val rulesCNF = (rules -- terminalProductions.values.toSet.flatten - toSubstitude._1).map { case (k, v) =>
    k -> (v.map(list => list.map(e => if (e == toSubstitude._1) toSubstitude._2 else e)))
  }
  
  //println(rulesCNF.size, rules.size, rulesCNF("0"))
  
  case class Prod2(a: String, b: String)
  def createNtp(rs: Map[String, Set[List[String]]]): Map[Prod2, Set[String]] = {
    rs.foldLeft(
      Map.empty[Prod2, Set[String]].withDefaultValue(Set.empty[String])
    ) { case (acc, (k, set)) =>
      val p2s = set.map { case a :: b :: Nil => Prod2(a, b) }
      p2s.foldLeft(acc) { (m, p2) => 
        val old = m(p2)
        val newSet = old + k
        m + (p2 -> newSet)
      }
    }
  }
  val nonTerminalProductions = createNtp(rulesCNF)
  
  def printArr(a: Array[Array[Set[String]]]): Unit = {
    (0 until a.length).foreach { row =>
      (0 until a(0).length).foreach { col =>
        print(a(row)(col).mkString("(", ",", ") "))
      }
      println()
    }
  }
  
  def cyk(word: String, initialProduction: String, terminalProductions: Map[Char, Set[String]], nonTerminalProductions: Map[Prod2, Set[String]]): Boolean = {
    val n = word.length
    val arr = Array.fill(n)(Array.fill(n)(Set.empty[String]))
  
    // fill in the 1st row
    (0 until n).foreach { i =>
      arr(0)(i) = terminalProductions.getOrElse(word(i), Set.empty[String])
    }
  
    (1 until n).foreach { row =>
      (0 until (n - row)).foreach { col =>
        (0 until row).foreach { i =>
          val upDownElem = arr(i)(col)
          val rightUpCornerElem = arr(row - 1 - i)(col + 1 + i)
          val prod2Set = 
            for {
              ud <- upDownElem
              corner <- rightUpCornerElem
            } yield Prod2(ud, corner)
          arr(row)(col) = arr(row)(col) ++ prod2Set.flatMap(p2 => nonTerminalProductions.getOrElse(p2, Set.empty[String]))
        }
      }
    }
  
    //printArr(arr)
    arr(n-1)(0).contains(initialProduction)
  }

  def run1(): Unit = {
    val resultsF = msgs.zipWithIndex.map { case (m, i) =>
      Future {
        val inGrammar = cyk(m, "0", terminalProductions, nonTerminalProductions)
        //println(s"$i: $inGrammar: $m")
        inGrammar
      }
    }
    val results = Await.result(Future.sequence(resultsF), Duration.Inf)
    val res = results.count(_ == true)
    println(res)
  }

  def run2(): Unit = {
    val rs = rulesCNF ++ Map(
      "8" -> Set(List("19", "84"), List("62", "47"), List("42", "8")),
      "500" -> Set(List("11", "31")),
      "0" -> Set(List("8", "11")),
      "11" -> Set(List("42", "31"), List("42", "500"))
    )
    val ntp = createNtp(rs)
    val resultsF = msgs.zipWithIndex.map { case (m, i) =>
      Future {
        val inGrammar = cyk(m, "0", terminalProductions, ntp)
        //println(s"$i: $inGrammar: $m")
        inGrammar
      }
    }
    val results = Await.result(Future.sequence(resultsF), Duration.Inf)
    val res = results.count(_ == true)
    println(res)
  }
}

Main.run1()
Main.run2()

