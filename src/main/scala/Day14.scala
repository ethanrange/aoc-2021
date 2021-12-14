import scala.io.Source

class Day14 {
  val lines = Source
    .fromFile("src/main/data/day14/input.txt")
    .getLines

  val tmpl = lines.next
  val rules = lines.drop(1).map(l => {
      val Array(k, v): Array[String] = l.split(" -> ")
      (k, v.head)
    }).toMap

  val cache = collection.mutable.Map.empty[(String, Int), Map[Char, BigInt]]

  def merge(m1: Map[Char, BigInt], m2: Map[Char, BigInt], dec: Char) = { 
    val mm = m1 ++ m2.map { case (ch, ct) => ch -> (ct + m1.getOrElse(ch, 0))}
    mm.updated(dec, mm(dec) - 1)
  }

  def comp(p: String, s: Int): Map[Char, BigInt] = cache.getOrElseUpdate(
    (p, s), {
      if (s > 0) {
        val res = rules(p)
        merge(comp(s"${p(0)}${res}", s - 1), comp(s"${res}${p(1)}", s - 1), res)
      } else p.groupBy(identity).transform((_, v) => v.size)
    })

  def computeSimulation(start: String, steps: Int): BigInt = {
    val cm = start.sliding(2).foldLeft(Map(): Map[Char, BigInt])((am, p) =>
            merge(am, comp(p, steps), p.head))

    cm.values.max - cm.values.min
  }

  def star1(): BigInt = computeSimulation(tmpl, 10)

  def star2(): BigInt = computeSimulation(tmpl, 40)
}
