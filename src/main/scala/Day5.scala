import scala.io.Source

class Day5 {
  val lines = Source
    .fromFile("src/main/data/day5/input.txt")
    .getLines
    .map { l =>
      l.split(" -> ").map(_.split(",")) match {
        case Array(Array(sx, sy), Array(ex, ey)) =>
          ((sx.toInt, sy.toInt), (ex.toInt, ey.toInt))
      }
    }
    .toSeq

  def computeOverlaps(list: Seq[((Int, Int), (Int, Int))], diag: Boolean): Int =
    list.flatMap { case ((sx, sy), (ex, ey)) =>
        if (sx == ex || sy == ey || (diag && (sx - ex).abs == (sy - ey).abs)) {
          (sx to ex by (if (sx < ex) 1 else -1))
          .zipAll(sy to ey by (if (sy < ey) 1 else -1), ex, ey)
        } else Seq.empty
      }
      .groupBy(identity)
      .count { case (x, l) => l.size > 1 }

  def star1(): Int = {
    computeOverlaps(lines, false);
  }

  def star2(): Int = {
    computeOverlaps(lines, true);
  }
}
