import scala.io.Source

class Day20 {
  val lines = Source.fromFile("src/main/data/day20/input.txt").getLines
  val algorithm = lines.next
  val pixelMap = lines.drop(1).zipWithIndex.flatMap { case (l, r) => 
    l.zipWithIndex.collect { case (p, c) => (r, c) -> (p == '#') }
  }.toMap
  val max = pixelMap.flatMap { case ((i, j), _) => Seq(i, j) }.max

  def enhance(original: Map[(Int, Int), Boolean], bounds: (Int, Int),
              default: Boolean): Map[(Int, Int), Boolean] = bounds match {
    case (lb, ub) => (for (i <- lb to ub; j <- lb to ub) yield (i, j))
      .foldLeft(Map[(Int, Int), Boolean]()){ case (m, (i, j)) => {
        val ix = (for (si <- i - 1 to i + 1; sj <- j - 1 to j + 1) 
          yield (original.getOrElse((si, sj), default)))
          .foldLeft(0)((s, b) => (s << 1) + (if (b) 1 else 0))

        m.updated((i, j), algorithm(ix) == '#')
      }
    }
  }

  def runSteps(board: Map[(Int, Int), Boolean], iterations: Int): Int =
    (1 to iterations).foldLeft(pixelMap)((m, i) => 
      enhance(m, (0 - i, max + i), algorithm.head == '#' && i % 2 == 0)
    ).count(_._2)

  def star1(): Int = runSteps(pixelMap, 2)

  def star2(): Int = runSteps(pixelMap, 50)
}