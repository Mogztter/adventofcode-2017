object Day14 {

  def main(args: Array[String]): Unit = {
    println(BigInt("a0c2017", 16).toString(2))
    diskDefragmentation("flqrgnkx")
  }

  def diskDefragmentation(input: String) = {
    val result = (1 to 127).map { value =>
      BigInt(Day10.knotHash(s"$input-$value"), 16).toString(2)
    }
    println(result)
  }
}
