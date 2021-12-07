import scala.io.Source

class Day7 {
  val first = Source.fromFile("src/main/data/day7/input.txt").getLines.toSeq(0)
  val digits = first.split(",").map(_.toInt).toSeq

  def star1(): Int = {
    val median = digits.sortWith(_ < _).drop(digits.length / 2).head
    digits.map(d => (d - median).abs).sum
  }

  def star2(): Int = {
    val mean = digits.sum / digits.length
    Seq(mean, mean + 1).map(m => digits.map(v => {
        val dif = (v - m).abs
        (dif * (dif + 1)) / 2
    }).sum).min
  }
}
