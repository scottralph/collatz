package collatz

import scala.Console.in

class DigitsTest extends org.scalatest.flatspec.AnyFlatSpec {
  behavior of "Digits"
  it should "display two digit values properly" in {
    val digits = new Digits(Array[Byte](4.toByte, 2.toByte))
    val output = digits.toString()
    assert(output.equals("0: 42\n"))
  }

  it should "give an incremented value" in {
    val digits = new Digits(Array[Byte](4.toByte, 2.toByte))
    val incremented = digits.addOne()
    assert(incremented.equals(new Digits(Array[Byte](4.toByte, 3.toByte))))
  }

  it should "perform the carry" in {
    val digits = new Digits(Array[Byte](2.toByte, 9.toByte, 9.toByte))
    val incremented = digits.addOne()
    assert(incremented.equals(new Digits(Array[Byte](3.toByte, 0.toByte, 0.toByte))))
  }

  it should "perform the carry and increase length" in {
    val digits = new Digits(Array[Byte](9.toByte, 9.toByte))
    val incremented = digits.addOne()
    assert(incremented.equals(new Digits(Array[Byte](1.toByte, 0.toByte, 0.toByte))))
  }

  it should "multiply by 3 properly" in {
    val digits = new Digits(Array[Byte](3.toByte, 5.toByte))
    val mult = digits.multiplyDigit(3)
    assert(mult.equals(new Digits(Array[Byte](1.toByte, 0.toByte, 5.toByte))))
  }

  it should "half numbers that result in a result of the same length" in {
    val digits = new Digits(Array[Byte](8.toByte, 2.toByte))
    val half = digits.half()
    assert(half.equals(new Digits(Array[Byte](4.toByte, 1.toByte))))
  }

  it should "half numbers that result in a shorter value" in {
    val digits = new Digits(Array[Byte](1.toByte, 6.toByte))
    val half = digits.half()
    assert(half.equals(new Digits(Array[Byte](8.toByte))))
  }

  it should "return 94 for next of 31" in {
    val digits = new Digits(Array[Byte](3.toByte, 1.toByte))
    val n = digits.next()
    assert(n.equals(new Digits(Array[Byte](9.toByte,4.toByte))))
  }


}
