import scala.io.Source

class Day17 {
  val input = Source.fromFile("src/main/data/day17/input.txt").getLines.next
  val Array((xs, xe), (ys, ye)) = input.substring(15).split(", y=").map { v =>
    val Array(s, e) = v.split("\\.\\.").map(_.toInt)
    (s, e)
  }
  
  val ps = for (i <- 0 to xe; j <- ys to (-ys - 1)) yield (i, j)

  def collides(i: Int, j: Int): (Int, Boolean) = {
    val ts = (0 to 250).scanLeft((0, 0, i, j)) { case ((x, y, vx, vy), _) => 
      (x + vx, y + vy, (vx - 1) max 0, vy - 1)
    }

    (ts.map(_._2).max, ts.exists { case (x, y, _, _) => 
      x >= xs && x <= xe && y >= ys && y <= ye })
  }

  def star1(): Int = ps.map { case (i, j) => 
    collides(i, j) }.filter(_._2).map(_._1).max

  def star2(): Int = ps.count { case (i, j) => collides(i, j)._2 }
}
