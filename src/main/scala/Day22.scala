import scala.io.Source

class Day22 {
  type Cuboid = (Boolean, Seq[Int])
  val cs = Source.fromFile("src/main/data/day22/input.txt").getLines.map { l => 
    l.split(" |=|,|\\.\\.") match { 
      case Array(t, rs @ _*) => (t == "on", rs.flatMap(s => s.toIntOption))
    }}.toSeq

  // Ugly but much faster than zip
  def overlap(f: Cuboid, s: Cuboid): Option[Cuboid] = {
    val ((t, Seq(x, ux, y, uy, z, uz)), (_, Seq(a, ua, b, ub, c, uc))) = (f, s)
    val r @ Seq(sx, ex, sy, ey, sz, ez) = 
        Seq(x max a, ux min ua, y max b, uy min ub, z max c, uz min uc)
    if (sx > ex || sy > ey || sz > ez) None else Some(!t, r)
  }

  def cbs(ls: Seq[Cuboid]): BigInt = ls.foldLeft((BigInt(0), Seq[Cuboid]())) {
    case ((ac, us), nc) => {
      val nx = us.flatMap(overlap(_, nc)) ++ (if (nc._1) Seq(nc) else Seq())
      (ac + nx.map{ case (t, r) => { r.grouped(2).map { case Seq(l, u) => 
        BigInt(u - l + 1) }.product * (if (t) 1 else -1) } }.sum, us ++ nx)
    }}._1

  def star1(): BigInt = cbs(cs.filter { case (t, Seq(x, ux, y, uy, z, uz)) =>
      !(x > 50 || ux < -50 || y > 50 || uy < -50 || z > 50 || uz < -50)})

  def star2(): BigInt = cbs(cs)
}
