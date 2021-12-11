import scala.io.Source

class Day11 {
  val grid = Source
    .fromFile("src/main/data/day11/input.txt")
    .getLines
    .map(_.toCharArray.map(_.toInt - '0'))
    .toArray

  def neighbours(i: Int, j: Int) =
    for (x <- i - 1 to i + 1; y <- j - 1 to j + 1; if (x != i || y != j) &&
        x >= 0 && y >= 0 && x < grid.size && y < grid.head.size) yield (x, y)

  def applyStep(grid: Array[Array[Int]]): (Int, Array[Array[Int]]) = {
    var res = grid.map(_.clone)
    var zeros: Seq[(Int, Int)] = Seq()

    for (i <- 0 until grid.size; j <- 0 until grid.head.size) res(i)(j) += 1

    while (res.exists(_.exists(_ > 9))) {
      for (i <- 0 until grid.size; j <- 0 until grid(0).size if res(i)(j) > 9) {
        zeros :+= (i, j)
        res(i)(j) = 0
        neighbours(i, j).foreach { case (x, y) => res(x)(y) += 1 }
      }
    }

    zeros.foreach { case (x, y) => res(x)(y) = 0 }
    (zeros.size, res)
  }

  def star1(): Int = (0 to 100).foldLeft((0, grid)) { case ((cv, cg), _) => {
        val (a, ng) = applyStep(cg)
        (cv + a, ng)
    }}._1

  def star2(): Int = {
    var state = (grid.map(_.clone), 0, 0)
    while (state._2 != grid.size * grid.head.size) {
      val (r, n) = applyStep(state._1)
      state = (n, r, state._3 + 1)
    }

    state._3
  }
}
