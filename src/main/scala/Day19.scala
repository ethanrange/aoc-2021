import scala.io.Source

class Day19 {
  type Point = (Int, Int, Int)

  val lines = Source.fromFile("src/main/data/day19/input.txt").getLines.toSeq
  var ofs = Map(0 -> (0, 0, 0))
  var ps = (for (l <- 0 until lines.size if lines(l).startsWith("--")) yield l)
    .map { l => lines.drop(l + 1).takeWhile(!_.isEmpty).map { m =>
        val Array(x, y, z) = m.split(',').map(_.toInt)
        (x, y, z)
      }
    }

  def pApp(tps: (Point, Point), f: Int => Int => Int) = tps match {
    case ((ax, ay, az), (bx, by, bz)) => (f(ax)(bx), f(ay)(by), f(az)(bz))
  }

  def rotations(ls: Point): Seq[Point] = ls match { case (x, y, z) => 
    Seq(( x, y, z), ( x, z,-y), ( x,-y,-z), ( x,-z, y), (-x, z, y), (-x,-z,-y),
        (-x,-y, z), (-x, y,-z), ( y,-x, z), ( y, x,-z), ( y, z, x), ( y,-z,-x),
        (-y, x, z), (-y,-x,-z), (-y, z,-x), (-y,-z, x), ( z, x, y), ( z, y,-x),
        ( z,-x,-y), ( z,-y, x), (-z, y, x), (-z,-y,-x), (-z,-x, y), (-z, x,-y))
  }

  def comp(as: Seq[Point], bs: Seq[Point]): Option[(Seq[Point], Point)] = {
    val brs = bs.map(rotations(_))

    (0 to 23).map(r => {
      val bds = (for (i <- 0 until bs.size; j <- 0 until bs.size if i != j)
        yield pApp((brs(i)(r), brs(j)(r)), x => y => x - y) -> i).toMap

      val ac = as.flatMap { a =>
        val ads = for (oa <- as if oa != a) yield pApp((a, oa), x => y => x - y)
        val bis = ads.flatMap(bds.get(_))
        if (bis.size >= 11) Some(a -> brs(bis.head)(r)) else None
      }

      if (ac.size >= 12) {
        val offset = pApp(ac.head, x => y => x - y)
        Some(brs.map(b => pApp((b(r), offset), x => y => x + y)), offset)
      } else None
    }).flatten.headOption
  }

  def star1(): Int = {
    while (ofs.size < ps.size) {
      for (next <- 1 until ps.size if !ofs.contains(next)) {
        comp(ps.head, ps(next)) match {
          case Some((nps, off)) => {
            ps = ps.updated(0, ps.head ++ nps)
            ofs += next -> off
          }
          case _ => ()
        }
      }
    }

    ps.head.distinct.size
  }

  def star2(): Int = (for ((_, (ax, ay, az)) <- ofs; (_, (bx, by, bz)) <- ofs)
      yield (ax - bx).abs + (ay - by).abs + (az - bz).abs).max
}
