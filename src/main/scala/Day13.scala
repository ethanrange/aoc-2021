import scala.io.Source

class Day13 {
  val lines = Source
    .fromFile("src/main/data/day13/input.txt")
    .getLines

  val coords = lines.takeWhile(!_.isBlank()).map(l => {
      val Array(as, bs) = l.split(',')
      (as.toInt, bs.toInt)
    }).toSeq

  val instrs = lines.map(l => {
      val Array(ax, v) = l.split('=')
      (ax.last, v.toInt)
    }).toSeq

  def fold(axis: Char, value: Int, dots: Seq[(Int, Int)]) = dots.map {
    case (x, y) => {
      if (axis == 'x' && x > value) (2 * value - x, y)
      else if (axis == 'y' && y > value) (x, 2 * value - y)
      else (x, y)
    }
  }

  def printBoard(dots: Seq[(Int, Int)]) = {
    for (i <- 0 to dots.maxBy(_._2)._2) {
      for (j <- 0 to dots.maxBy(_._1)._1) {
        if (dots contains (j, i)) print("# ") else print(". ")
      }
      print("\n")
    }
  }

  def star1(): Int = fold(instrs(0)._1, instrs(0)._2, coords).distinct.size

  def star2(): Unit = printBoard(instrs.foldLeft(coords) { case (cb, (ax, v)) =>
    fold(ax, v, cb)
  })
}
