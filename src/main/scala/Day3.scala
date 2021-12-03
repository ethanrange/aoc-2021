import scala.io.Source

class Day3 {
  val lines = Source.fromFile("src/main/data/day3/input.txt").getLines.toSeq
  val length = lines(0).size

  def counts(list: Seq[String]): Seq[Char] = {
    val half = list.size / 2f

    list
      .map(_.toList)
      .transpose
      .map { l => if (l.count(_ == '1') >= half) '1' else '0' }
  }

  def toMin(list: Seq[Char]) = list.map { c => if (c == '1') '0' else '1' }

  def star1(): Int = {
    val most = counts(lines)

    Integer.parseInt(most.mkString, 2) * Integer.parseInt(
      toMin(most).mkString,
      2
    )
  }

  def star2(): Int = {
    var mRes, lRes = lines
    var i = 0

    for (i <- 0 until length) {
      if (mRes.size > 1) {
        val mVals = counts(mRes)
        mRes = mRes.filter(_(i) == mVals(i))
      }

      if (lRes.size > 1) {
        val lVals = toMin(counts(lRes))
        lRes = lRes.filter(_(i) == lVals(i))
      }
    }

    Integer.parseInt(mRes.mkString, 2) * Integer.parseInt(lRes.mkString, 2)
  }
}
