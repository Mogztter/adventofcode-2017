import scala.util.matching.Regex

object Day13 {

  def main(args: Array[String]): Unit = {
    packetScanners("0: 3\n1: 2\n2: 9\n4: 4\n6: 4\n8: 6\n10: 6\n12: 8\n14: 5\n16: 6\n18: 8\n20: 8\n22: 8\n24: 6\n26: 12\n28: 12\n30: 8\n32: 10\n34: 12\n36: 12\n38: 10\n40: 12\n42: 12\n44: 12\n46: 12\n48: 14\n50: 14\n52: 8\n54: 12\n56: 14\n58: 14\n60: 14\n64: 14\n66: 14\n68: 14\n70: 14\n72: 14\n74: 12\n76: 18\n78: 14\n80: 14\n86: 18\n88: 18\n94: 20\n98: 18")
  }

  case class Firewall(depth: Int, range: Int)

  val pattern: Regex = "([0-9]+): ([0-9]+)".r

  def packetScanners(input: String): Unit = {
    val firewalls = input.split("\n").map { line =>
      val pattern(depth, range) = line
      val firewall = Firewall(depth.toInt, range.toInt)
      firewall.depth -> firewall
    }.toMap
    println(severity(firewalls, 0))
    println(Stream.from(0).find(delay => caught(firewalls, delay).isEmpty).get)
  }

  def caught(firewalls: Map[Int, Firewall], delay: Int): Seq[Firewall] = {
    (0 to firewalls.keys.max).flatMap { time =>
      firewalls.get(time).filter { firewall =>
        scannerPosition(firewall, time + delay) == 0
      }
    }
  }

  def severity(firewalls: Map[Int, Firewall], delay: Int): Int = {
    caught(firewalls, delay).map {firewall =>
      firewall.range * firewall.depth
    }.sum
  }

  def scannerPosition(firewall: Firewall, time: Int): Int = {
    val offset = time % ((firewall.range - 1) * 2)
    if (offset > firewall.range - 1) {
      2 * (firewall.range - 1) - offset
    } else {
      offset
    }
  }
}