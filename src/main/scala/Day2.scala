import scala.io.Source

class Day2 {
  val lines = Source
    .fromFile("src/main/data/day2/input.txt")
    .getLines
    .map { s => s.split(" ") match { case Array(c, v) => (c, v.toInt) } }
    .toSeq

  def star1(): Int = {
    val (h, d) = lines.foldLeft((0, 0)) { case ((ch, cv), (c, v)) =>
      c match {
        case "forward" => (ch + v, cv)
        case "up"      => (ch, cv - v)
        case "down"    => (ch, cv + v)
      }
    }

    h * d
  }

  def star2(): Int = {
    val (h, d, a) = lines.foldLeft((0, 0, 0)) { case ((ch, cv, ca), (c, v)) =>
      c match {
        case "forward" => (ch + v, cv + v * ca, ca)
        case "up"      => (ch, cv, ca - v)
        case "down"    => (ch, cv, ca + v)
      }
    }

    h * d
  }
}
