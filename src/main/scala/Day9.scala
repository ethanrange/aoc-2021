import scala.io.Source

class Day9 {
  val lines = Source
    .fromFile("src/main/data/day9/alternate.txt")
    .getLines
    .map(_.toCharArray.map(_.toInt - '0').toSeq)
    .toSeq

  val indicies = (0 until lines.size).flatMap(i =>
    (0 until lines.head.size).map(j => (i, j))
  )

  def neighbours(i: Int, j: Int): Seq[(Int, Int)] =
    Seq((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)).filter { case (x, y) =>
      x >= 0 && x < lines.size && y >= 0 && y < lines.head.size
    }

  def basin(i: Int, j: Int): Seq[(Int, Int)] = {
    neighbours(i, j)
      .filter { case (x, y) => lines(i)(j) < lines(x)(y) && lines(x)(y) != 9 }
      .flatMap { case (x, y) => basin(x, y) } :+ (i, j)
  }

  def star1(): Int = {
    indicies.filter { case (i, j) =>
        neighbours(i, j).forall { case (x, y) => lines(i)(j) < lines(x)(y) }
      }.map { case (a, b) => lines(a)(b) + 1 }.sum
  }

  def star2(): Int = {
    indicies.map { case (x, y) => basin(x, y).distinct.size }.sortWith(_ > _)
      .take(3).product
  }
}
