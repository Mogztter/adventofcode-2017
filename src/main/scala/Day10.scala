object Day10 {

  val suffix = Seq(17, 31, 73, 47, 23)

  def main(args: Array[String]): Unit = {
    val input = "157,222,1,2,177,254,0,228,159,140,249,187,255,51,76,30"
    println(knotHash(input))
  }

  def knotHash(input: String): String = {
    val asciiInput = toASCII(input)

    var list = (0 to 255).toArray
    var position = 0
    var skip = 0

    (1 to 64).foreach { round =>
      asciiInput.foreach { size =>
        list = reverse(list, position, position + size)
        position = (position + size + skip) % list.length
        skip += 1
      }
    }
    val result = denseHash(list.toSeq)
    println(result.toArray.toSeq)
    toHex(result)
  }

  def toHex(list: Seq[Int]): String = {
    list.map { number =>
      val value = Integer.toHexString(number)
      if (value.length == 1) {
        s"0$value"
      } else {
        value
      }
    }.mkString("")
  }

  def denseHash(list: Seq[Int]): Seq[Int] = {
    list.grouped(16).map { block =>
      block.reduce(_ ^ _)
    }.toSeq
  }

  def reverse(list: Array[Int], start: Int, end: Int): Array[Int] = {
    val reversed = (if (end > list.length) {
      list.slice(start, list.length) ++ list.slice(0, end - list.length)
    } else {
      list.slice(start, end)
    }).reverse
    list.zipWithIndex.map { case (value, index) =>
      if (end > list.length) {
        if (index < end - list.length) {
          reversed(index + (list.length - start))
        } else if (index >= start && index <= list.length) {
          reversed(index - start)
        } else {
          value
        }
      } else if (index >= start && index < end) {
        reversed(index - start)
      } else {
        value
      }
    }
  }

  def asciiMap: Map[Char, Int] = {
    val asciiMapNumeric = ('0' to '9').zipWithIndex.map { case (letter, index) =>
      letter -> (48 + index)
    }
    val asciiMapLetter = ('a' to 'z').zipWithIndex.map { case (letter, index) =>
      letter -> (97 + index)
    }
    (asciiMapNumeric ++ asciiMapLetter).toMap + (',' -> 44) + ('-' -> 45)
  }

  def toASCII(input: String): Seq[Int] = input.map(char => asciiMap(char)) ++ suffix
}