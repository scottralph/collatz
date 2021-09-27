package collatz

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

class Digits(val digitSequence : Array[Byte]) {
  val OUTPUT_DIGIT_WIDTH = 100

  override def toString(): String = {
    val sb = new StringBuilder()
    for (i <- 0 until (digitSequence.length) by OUTPUT_DIGIT_WIDTH) {
      i.toString().foreach(c => sb += c)
      sb += ':'
      sb += ' '
      val m = (i + OUTPUT_DIGIT_WIDTH).min(digitSequence.length)
      for (j <- i until m) {
        digitSequence(j).toInt.toString.foreach(c => sb += c)
      }
      sb += '\n'
    }
    sb.toString()
  }

  def asHashString() : String = {
    digitSequence.map(x => x.toInt.toString()).toArray.mkString
  }

  def addOne(): Digits = {
    if (digitSequence.forall(x => x.equals(9.toByte))) {
      val ret = Array.fill[Byte](digitSequence.length + 1)(0.toByte)
      ret(0) = 1
      new Digits(ret)
    } else {
      var carry = true
      val d = new Array[Byte](digitSequence.length)
      for (i <- digitSequence.length - 1 to 0 by -1) {
        if (digitSequence(i).equals(9.toByte) && carry) {
          d(i) = 0
        } else {
          if (carry) {
            d(i) = (digitSequence(i) + 1.toByte).toByte
          } else {
            d(i) = digitSequence(i)
           }
          carry = false
        }
      }
      new Digits(d)
    }
  }

  def multiplyDigit(scalar : Integer): Digits = {
    var posn = digitSequence.length - 1
    var carry : Integer = 0
    val outputDigits = ListBuffer[Byte]()

    while ((posn >= 0) || (carry != 0)) {
      val digit = if (posn >= 0) digitSequence(posn).toInt else 0
      val mult: Integer = digit * scalar + carry
      val digitOut = (mult % 10).toByte
      posn = posn - 1
      carry = mult / 10
      outputDigits += digitOut
    }
    val dig = outputDigits.reverse.toArray
    new Digits(dig)
  }

  def half() : Digits = {
    val outputDigits = ListBuffer[Byte]()
    val len = digitSequence.length
    var carry = 0
    for (i <- 0 until len) {
      val currentDigit = digitSequence(i).toInt
      val div = (carry * 10) + currentDigit
      val outputDig = div / 2
      carry = div % 2
      if (i > 0 || outputDig > 0)
        outputDigits += outputDig.toByte
    }
    new Digits(outputDigits.toArray)
    }

  def next() : Digits = {
    if ((digitSequence(digitSequence.length - 1).toInt % 2) == 0)
      this.half()
    else
      this.multiplyDigit(3).addOne()
  }

  def copy() : Digits = new Digits(this.digitSequence)

  def stoppingTime(debug : Boolean = false) : Integer = {
    var seen = Set.empty[String]
    var count = 0
    var x = this.copy()
    while(!x.isUnity()) {
      val hashString = x.asHashString()
      if (seen(hashString)) {
        println("*************** FOUND A CYCLE")
        return -1
      }
      seen += hashString

      count +=1
      x = x.next()
      if(debug)
        println(s"At iteration $count the magnitude ${x.magnitude()}")
    }
    return count
  }

  def isUnity() : Boolean = digitSequence.length == 1 && digitSequence(0) == 1.toByte

  def magnitude() : Int = digitSequence.length


  def canEqual(other: Any): Boolean = other.isInstanceOf[Digits]

  override def equals(other: Any): Boolean = other match {
    case that: Digits =>
      if(!that.canEqual(this))
        {
          return false
        }
      if (digitSequence.length != that.digitSequence.length) {
        return false
      }
      return (0 to (digitSequence.length - 1)).forall(i => digitSequence(i) == that.digitSequence(i))

    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(digitSequence)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Digits {
  def rand_digit(posn: Integer) : Byte = {
    val r = scala.util.Random
    var digit = r.nextInt(10)
    while(posn == 0 && digit == 0) {
      digit = r.nextInt()
    }
    digit.toByte
  }

  def apply(length: Integer): Digits = {
    val digArray = (0 until length).map(i => rand_digit(i)).toArray
    new Digits(digArray)
  }
}
