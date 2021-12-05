import scala.io.Source

class Day4 {
  val lines = Source.fromFile("src/main/data/day4/input.txt").getLines.toSeq
  var calls = lines(0).split(",").toList
  var boards: Seq[Seq[Seq[String]]] = Seq()

  for (l <- 1 until lines.size if lines(l).trim.isEmpty) {
    val board = (1 to 5).map { i => lines(l + i).trim.split(" +").toSeq }
    boards = boards :+ board
  }

  def checkBoard(board: Seq[Seq[String]]): Boolean =
    board.exists(_.forall(_ == "T")) ||
    board.transpose.exists(_.forall(_ == "T"))

  def sumBoard(board: Seq[Seq[String]]): Int =
    board.map(_.map { e => if (e == "T") 0 else e.toInt }.sum).sum

  def applyNext(): Int = {
    val next :: rcalls = calls
    calls = rcalls

    boards = boards.map(_.map(_.map { e => if (e == next) "T" else e }))
    next.toInt
  }

  def star1(): Int = {
    var last = 0

    while (!boards.exists(checkBoard(_))) {
      last = applyNext();
    }

    last * sumBoard(boards.filter(checkBoard(_))(0))
  }

  def star2(): Int = {
    var last = 0
    var fin = boards(0)

    while (boards.size > 0) {
      last = applyNext();

      fin = boards(0)
      boards = boards.filter(!checkBoard(_))
    }

    last * sumBoard(fin)
  }
}
