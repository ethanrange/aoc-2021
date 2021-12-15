import scala.io.Source

class Day15 {
  type Grid = Array[Array[Int]]

  val grid = Source
    .fromFile("src/main/data/day15/input.txt")
    .getLines
    .map(_.toCharArray.map(_.toInt - '0'))
    .toArray

  def minr(cg: Grid, i: Int, j: Int, vs: Set[(Int, Int)], bs: Set[(Int, Int)],
           lg: Grid): (Grid, Set[(Int, Int)], Set[(Int, Int)]) = {
    var ng = cg.map(_.clone)
    var nb = bs

    Seq((-1, 0), (1, 0), (0, -1), (0, 1)).foreach { case (di, dj) => {
        val p @ (nx, ny) = (i + di, j + dj)
        if (!vs.contains(p) && nx >= 0 && nx < lg.head.size && ny >= 0 &&
          ny < lg.size) {
          ng(ny)(nx) = ng(ny)(nx).min(ng(j)(i) + lg(ny)(nx))
          nb += (nx -> ny)
        }
      }
    }

    (ng, vs + (i -> j), nb)
  }

  def compute(reps: Int) = {
    var ng = Array.tabulate(grid.size * reps, grid.head.size * reps)((x, y) => {
      val add = (x / grid.size) + (y / grid.head.size)
      1 + ((grid(x % grid.size)(y % grid.head.size) + add - 1) % 9)
    })

    val res = Array.tabulate(ng.size, ng.head.size)((x, y) =>
      if (x == 0 && y == 0) 0 else Int.MaxValue)
    var state = (res, Set[(Int, Int)](), Set[(Int, Int)]((0, 0)))

    while (!state._3.isEmpty) {
      val (i, j) = state._3.minBy { case (i, j) => state._1(j)(i) }
      state = minr(state._1, i, j, state._2, state._3 - ((i, j)), ng)
    }

    state._1.last.last
  }

  def star1(): Int = compute(1)

  def star2(): Int = compute(5)
}
