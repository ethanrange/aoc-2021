import scala.io.Source

class Day16 {

  val input = Source.fromFile("src/main/data/day16/input.txt").getLines.next
  val bin = input.map(c => String
        .format("%4s", BigInt(c.toString(), 16).toString(2))
        .replace(" ", "0")
    ).mkString

  def parsePacket(tms: String): (BigInt, BigInt, String) = {
    val (vs, vrem) = tms.splitAt(3)
    val (ts, trem) = vrem.splitAt(3)
    var frem = trem
    var vsum = BigInt(vs, 2)
    var fres = BigInt(0)

    ts match {
      case "100" => {
        var res = ""
        var n = ""
        val groups = trem.grouped(5)
      
        do {
          n = groups.next
          res += n.tail
          frem = frem.substring(5)
        } while (n.head != '0')
        
        fres = BigInt(res, 2)
      }
      case t => {
        var (l, lrem) = trem.splitAt(1)
        var results: Seq[BigInt] = Seq()

        l match {
          case "0" => {
            var (tl, tlrem) = lrem.splitAt(15)
            var toRead = BigInt(tl, 2)
            var ssize = tlrem.size

            while (ssize - tlrem.size < toRead) {
              var (a, r, nxtrem) = parsePacket(tlrem)
              tlrem = nxtrem
              results = results :+ r
              vsum += a
            }

            frem = tlrem
          }
          case "1" => {
            var (sp, sprem) = lrem.splitAt(11)

            frem = (0 until Integer.parseInt(sp, 2)).foldLeft(sprem)((orem, _) => {
              val (a, r, nrem) = parsePacket(orem)
              vsum += a
              results = results :+ r
              nrem
            })
          }
        }

        fres = Integer.parseInt(t, 2) match {
          case 0 => results.sum
          case 1 => results.product
          case 2 => results.min
          case 3 => results.max
          case 5 => if (results(0) > results(1)) 1 else 0
          case 6 => if (results(0) < results(1)) 1 else 0
          case 7 => if (results(0) == results(1)) 1 else 0
        }
      }
    }

    (vsum, fres, frem)
  }

  def star1(): BigInt = parsePacket(bin)._1

  def star2(): BigInt = parsePacket(bin)._2
}
