import scala.io.Source

class Day8 {
  val lines = Source
    .fromFile("src/main/data/day8/input.txt")
    .getLines
    .map(l => {
      val Array(as, bs) = l.split('|').map(_.trim).map(_.split(" ").toSeq)
      (as, bs)
    })
    .toSeq

  def compute(as: Seq[String]): Seq[String] = {
    var pairings = Array.fill[Set[Char]](10)(Set())
    val freqs = as.map(_.toSet).groupBy(_.size)

    pairings(1) = freqs(2).head
    pairings(7) = freqs(3).head
    pairings(4) = freqs(4).head
    pairings(8) = freqs(7).head
    pairings(6) = freqs(6).find(!pairings(1).subsetOf(_)).get
    pairings(9) = freqs(6).find(pairings(4).subsetOf(_)).get
    pairings(0) = freqs(6).diff(Seq(pairings(6), pairings(9))).head
    pairings(3) = freqs(5).find(pairings(1).subsetOf(_)).get
    pairings(2) = freqs(5).find(_.diff(pairings(4)).size == 3).get
    pairings(5) = freqs(5).diff(Seq(pairings(3), pairings(2))).head

    pairings.map(_.mkString.sortWith(_ < _))
  }

  def star1(): Int = {
    lines.map { case (_, gs) =>
      gs.count(Seq(2, 3, 4, 7) contains _.length)
    }.sum
  }

  def star2(): Int = {
    lines.map {
      case (ds, gs) => {
        val ps = compute(ds)
        gs.map(g => ps.indexOf(g.sortWith(_ < _))).mkString.toInt
      }
    }.sum
  }
}
