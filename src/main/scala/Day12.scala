import scala.io.Source

class Day12 {
  val paths = Source
    .fromFile("src/main/data/day12/input.txt")
    .getLines
    .flatMap(l => {
      val Array(as, bs) = l.split('-')
      Array((as, bs), (bs, as))
    })
    .toSeq

  def visit(cave: String, seen: Set[String], ex: Boolean): Int = {
    paths.collect[Int] { case (s, e) if s == cave => {
        if (e == "end") 1
        else if (seen.contains(e) && e.head.isLower)
          if (!ex && e != "start") visit(e, seen + s, true) else 0
        else visit(e, seen + s, ex)
      }
    }.sum
  }

  def star1(): Int = visit("start", Set(), true)

  def star2(): Int = visit("start", Set(), false)
}
