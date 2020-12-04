import scala.io.Source
import scala.util.Try

case class Input(xs: List[Map[String, String]] = List.empty, startNew: Boolean = true)

val input = 
  Source
    .fromFile("input")
    .getLines
    .foldLeft(Input()) { (acc, line) =>
      if (line.isEmpty) {
        acc.copy(startNew = true)
      } else {
        val kvLine = kvFromLine(line)
        if (acc.startNew) {
          Input(xs = kvLine :: acc.xs, startNew = false)
        } else {
          val newXs = (acc.xs.head ++ kvLine) :: acc.xs.tail
          acc.copy(xs = newXs)
        }
      }
    }

def kvFromLine(line: String): Map[String, String] = {
  line.split(' ').map(_.trim.split(':')).foldLeft(Map.empty[String, String]) { case (acc, Array(k, v)) =>
    acc + (k -> v)
  }
}

val Input(passports, _) = input
val validFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

val validCount = passports.count { p =>
  validFields.forall(p.contains)
}
println(validCount)

def isPassportValid(p: Map[String, String]): Boolean = {
  validFields.forall(p.contains) && 
    validRange(1920, 2002)(p("byr")) &&
    validRange(2010, 2020)(p("iyr")) &&
    validRange(2020, 2030)(p("eyr")) &&
    isValidHgt(p("hgt")) &&
    isValidHcl(p("hcl")) &&
    isValidEcl(p("ecl")) &&
    isValidPid(p("pid"))
}

def isValidHgt(x: String): Boolean = {
  if (x.endsWith("cm")) {
    validRange(150, 193)(x.dropRight(2))
  } else if (x.endsWith("in")) {
    validRange(59, 76)(x.dropRight(2))
  } else {
    false
  }
}

def isValidHcl(x: String): Boolean =
  x.startsWith("#") && x.drop(1).forall(c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))

def isValidEcl(x: String): Boolean =
  Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth") contains x

def isValidPid(x: String): Boolean =
  x.length == 9 && x.forall(c => c >= '0' && c <= '9')

def validRange(low: Int, high: Int)(x: String): Boolean =
  Try(x.toInt).map(n => n >= low && n <= high).getOrElse(false)

val validCount2 = passports.count(isPassportValid)
println(validCount2)

