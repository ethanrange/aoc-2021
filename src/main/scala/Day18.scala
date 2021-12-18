import scala.io.Source
import scala.util.matching.Regex

sealed trait SNum {
  def add(sn: SNum, v: Int, left: Boolean): SNum = sn match {
    case P(f, s) => if (left) P(add(f, v, left), s) else P(f, add(s, v, left))
    case S(s)    => S(s + v)
  }

  def tryExplode(sn: SNum, d: Int): (SNum, Boolean, (Int, Int)) = sn match {
    case S(_)                      => (sn, false, (0, 0))
    case P(S(v1), S(v2)) if d >= 4 => (S(0), true, (v1, v2))
    case p @ P(fp, sp) => {
      val (rf, fs, (fl, fr)) = tryExplode(fp, d + 1)
      val (rs, ss, (sl, sr)) = tryExplode(sp, d + 1)

      (fs, ss) match {
        case (true, _) => (P(rf, add(sp, fr, true)), fs, (fl, 0))
        case (_, true) => (P(add(fp, sl, false), rs), ss, (0, sr))
        case _         => (p, false, (0, 0))
      }
    }
  }

  def trySplit(sn: SNum): (SNum, Boolean) = sn match {
    case p @ P(fp, sp) => {
      val (rf, fs) = trySplit(fp)
      val (rs, ss) = trySplit(sp)

      (fs, ss) match {
        case (true, _) => (P(rf, sp), fs)
        case (_, true) => (P(fp, rs), ss)
        case _         => (p, false)
      }
    }
    case S(n) => {
      val h = n.toFloat / 2
      if (n >= 10) (P(S(h.floor.toInt), S(h.ceil.toInt)), true) else (sn, false)
    }
  }

  def reduce(sn: SNum): SNum = {
    val (er, es, _) = tryExplode(sn, 0)
    val (sr, ss) = trySplit(sn)

    (es, ss) match {
      case (true, _) => reduce(er)
      case (_, true) => reduce(sr)
      case _         => sn
    }
  }


  def +(that: SNum) = reduce(P(this, that))
}

final case class P(f: SNum, s: SNum) extends SNum
final case class S(v: Int) extends SNum

class Day18 {
  val lines = Source
    .fromFile("src/main/data/day18/input.txt")
    .getLines
    .map(parse(_))
    .toSeq

  def getPivot(s: String) = (0 until s.size).find(i => s(i) == ',' &&
    s.substring(1, i).count(_ == '[') == s.substring(1, i).count(_ == ']')).get

  def parse(s: String): SNum = 
    if (s.startsWith("[")) {
      val (p1, p2) = s.splitAt(getPivot(s))
      P(parse(p1.tail), parse(p2.tail.init))
    } else {
      S(s.toInt)
    }

  def mag(sn: SNum): Int = sn match {
    case P(p1, p2) => 3 * mag(p1) + 2 * mag(p2)
    case S(v)      => v
  }

  def star1(): Int = mag(lines.tail.foldLeft(lines.head)(_ + _))

  def star2(): Int = (for (x <- lines; y <- lines) yield (mag(x + y))).max
}
