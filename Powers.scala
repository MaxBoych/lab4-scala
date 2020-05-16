class Powers[T](f: T) {
    var factor: T = f

    def pow(number: Int): Any = {
        var n = number
        factor match {
            case _: Long =>
                var f = factor.asInstanceOf[Long]
                var res = f
                while (n != 0) {
                    val bitMult = n & 1
                    if (bitMult != 0) {
                        res *= f
                    }
                    f *= f
                    n >>= 1
                }
                res
            case _: String =>
                var f = factor.asInstanceOf[String]
                var res = f
                while (n != 0) {
                    val bitMult = n & 1
                    if (bitMult != 0) {
                        res += f
                    }
                    f += f
                    n >>= 1
                }
                res
            case _ =>
                throw new RuntimeException("illegal type")
        }
    }

    def *(multiplier: T): Unit = {
        factor match {
            case _: Long =>
                val f = factor.asInstanceOf[Long]
                val m = multiplier.asInstanceOf[Long]
                factor = (f * m).asInstanceOf[T]
            case _: String =>
                val f = factor.asInstanceOf[String]
                val m = multiplier.asInstanceOf[String]
                factor = (f + m).asInstanceOf[T]
            case _ =>
                throw new RuntimeException("illegal type")
        }
    }

    /*override def toString: String = {

    }*/
}

object Main {
    def main(args: Array[String]): Unit = {
        val powers = new Powers[Long](2)
        println(powers.pow(3).toString)
        powers * 15
        println(powers.pow(3).toString)

        val powers2 = new Powers[String]("lab4")
        println(powers2.pow(3).toString)
        powers2 * "scala"
        println(powers2.pow(3).toString)
    }
}
