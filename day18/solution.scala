import scala.io.Source
import scala.annotation.tailrec

sealed trait Expr
case class N(n: Long) extends Expr
case class Add(a: Expr, b: Expr) extends Expr
case class Mul(a: Expr, b: Expr) extends Expr

sealed trait Token
case class Operand(n: Long) extends Token
case object AddToken extends Token
case object MulToken extends Token
case class Priority(a: Token, b: Token) extends Token
case class Delay(s: String) extends Token
case class Parentheses(v: Vector[Token]) extends Token

@tailrec
def findParentheses(s: String, i: Int, open: Int, closed: Int, startingOpen: Int = -1, res: List[(Int, Int)] = List.empty): List[(Int, Int)] = {
  if (i >= s.length) res
  else if (s(i) == '(') {
    if (open > closed) findParentheses(s, i + 1, open + 1, closed, startingOpen, res)
    else { // new open
      findParentheses(s, i + 1, open + 1, closed, i, res)
    }
  }
  else if (s(i) == ')') {
    if (open - closed == 1) { // match found
      findParentheses(s, i + 1, 0, 0, -1, (startingOpen -> i) :: res)
    }
    else { // more closing needed
      findParentheses(s, i + 1, open, closed + 1, startingOpen, res)
    }
  }
  else findParentheses(s, i + 1, open, closed, startingOpen, res)
}

def splitIntoTokens(s: String, parentheses: List[(Int, Int)]): Vector[Token] = {
  if (parentheses.isEmpty) splitFlat(s)
  else {
    val (tokens, lastIndex) =
      parentheses.foldLeft((Vector.empty[Token], 0)) { case ((res, prevIndex), (a, b)) =>
        if (a - prevIndex > 1) {
          val newRes = res ++ splitFlat(s.substring(prevIndex, a)) ++ Vector(Delay(s.substring(a + 1, b)))
          (newRes, b + 1)
        }
        else {
          val newRes = res ++ Vector(Delay(s.substring(a + 1, b)))
          (newRes, b + 1)
        }
      }
    val fullTokens =
      if (lastIndex < s.length - 1) tokens ++ splitFlat(s.substring(lastIndex + 1))
      else tokens
    fullTokens
  }
}

def splitFlat(s: String): Vector[Token] =
  if (s.isEmpty) Vector.empty
  else {
    s.trim.split(' ').foldLeft(Vector.empty[Token]) { (acc, elem) =>
      //println(s"elem = $elem")
      val next = elem match {
        case "*" => MulToken
        case "+" => AddToken
        case n => Operand(n.toLong)
      }
      acc :+ next
    }
  }

def tokensFrom(s: String): Vector[Token] = {
  //println(s)
  val ranges = findParentheses(s, 0, 0, 0).reverse
  //println(ranges)
  val tokens = splitIntoTokens(s, ranges)
  //println(tokens)
  tokens.map {
    case Delay(string) => Parentheses(tokensFrom(string))
    case other => other
  }
}

def exprFrom(tokens: List[Token]): Expr = tokens.reverse match {
  case t :: Nil =>
    t match {
      case Operand(n) => N(n)
      case Parentheses(ts) => exprFrom(ts.toList)
    }

  case a :: AddToken :: other =>
    a match {
      case Operand(n) => Add(N(n), exprFrom(other.reverse))
      case Parentheses(ts) => Add(exprFrom(ts.toList), exprFrom(other.reverse))
    }

  case a :: MulToken :: other =>
    a match {
      case Operand(n) => Mul(N(n), exprFrom(other.reverse))
      case Parentheses(ts) => Mul(exprFrom(ts.toList), exprFrom(other.reverse))
    }
}

def maybePrioritize(p: Token): Token = p match {
  case Parentheses(v) => Parentheses(prioritize(v.toList).toVector)
  case other => other
}

def prioritize(tokens: List[Token]): List[Token] = tokens match {
  case Nil => Nil
  case a :: AddToken :: b :: other => prioritize(Priority(maybePrioritize(a), maybePrioritize(b)) :: other)
  case a :: MulToken :: other => maybePrioritize(a) :: MulToken ::prioritize(other)
  case other :: Nil => maybePrioritize(other) :: Nil
}

def priorityExprFrom(tokens: List[Token]): Expr = tokens.reverse match {
  case t :: Nil =>
    t match {
      case Operand(n) => N(n)
      case Parentheses(ts) => priorityExprFrom(ts.toList)
      case Priority(a, b) => Add(priorityExprFrom(List(a)), priorityExprFrom(List(b)))
    }

  case a :: MulToken :: other =>
    a match {
      case Operand(n) => Mul(N(n), priorityExprFrom(other.reverse))
      case Parentheses(ts) => Mul(priorityExprFrom(ts.toList), priorityExprFrom(other.reverse))
      case Priority(x, y) => Mul(Add(priorityExprFrom(List(x)), priorityExprFrom(List(y))), priorityExprFrom(other.reverse))
    }
}

def eval(e: Expr): Long= e match {
  case N(n) => n
  case Add(a, b) => eval(a) + eval(b)
  case Mul(a, b) => eval(a) * eval(b)
}

val ts = Source.fromFile("input").getLines.map(tokensFrom).toList
//ts.foreach(println)

val es = ts.map(t => exprFrom(t.toList))
//es.foreach(println)

val results = es.map(eval)
//results.foreach(println)

println(results.sum)

val ts2 = ts.map(_.toList).map(prioritize)
//ts2.foreach(println)
val es2 = ts2.map(t => priorityExprFrom(t.toList))
val results2 = es2.map(eval)
//results2.foreach(println)
println(results2.sum)

