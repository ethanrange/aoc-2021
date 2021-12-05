import scala.io.Source

class Day5 {
  val lines = Source
    .fromFile("src/main/data/day5/input.txt")
    .getLines
    .map(_.split(" -> |,").map(_.toInt) match {
      case Array(sx, sy, ex, ey) => ((sx, sy), (ex, ey))
    })
    .toSeq

  def computeOverlaps(list: Seq[((Int, Int), (Int, Int))], diag: Boolean): Int =
    list.flatMap { case ((sx, sy), (ex, ey)) =>
        if (sx == ex || sy == ey || (diag && (sx - ex).abs == (sy - ey).abs))
          (sx to ex by (if (sx < ex) 1 else -1))
            .zipAll(sy to ey by (if (sy < ey) 1 else -1), ex, ey)
        else Seq.empty
      }
      .groupBy(identity)
      .count { case (x, l) => l.size > 1 }

  def star1(): Int = computeOverlaps(lines, false)

  def star2(): Int = computeOverlaps(lines, true)
}
