import scala.io.Source
import scala.collection.mutable

class Day21 {
  val lines = Source.fromFile("src/main/data/day21/input.txt").getLines
  val is = lines.map(_.last - '0' -> 0).toSeq

  val deterministic = Iterator.iterate(1)(1 + _ % 100).grouped(3)
  val cache = mutable.Map.empty[(Seq[(Int, Int)], Int, Int), Seq[BigInt]]

  def split(s: Seq[(Int, Int)], roll: Int, index: Int): Seq[BigInt] =
    cache.getOrElseUpdate((s, roll, index), {
      val res = s.updated(index, s(index) match { case (p, s) => {
        val np = 1 + (p + roll - 1) % 10
        (np, s + np)
      }})

      if (res(index)._2 >= 21)
        Seq[BigInt](0, 0).updated(index, BigInt(1))
      else
        (for (i <- 1 to 3; j <- 1 to 3; k <- 1 to 3) yield i + j + k)
          .map(split(res, _, 1 - index)).transpose.map(_.sum)
    })

  def star1(): Int = {
    var state = (is, 0, 0)

    while (state._1(1 - state._2)._2 < 1000) {
      state = (state._1.updated(state._2, state._1(state._2) match {
                case (p, s) => {
                  val np = 1 + (p + deterministic.next.sum - 1) % 10
                  (np, s + np)
                }}), 1 - state._2, state._3 + 3)
    }

    state._1(state._2)._2 * state._3
  }

  def star2(): BigInt = (for (i <- 1 to 3; j <- 1 to 3; k <- 1 to 3) 
    yield i + j + k).map(split(is, _, 0)).transpose.map(_.sum).max
}
