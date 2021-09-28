package collatz
import collatz.Digits

import scala.sys.exit

object Main {
  def main(args: Array[String]) {
    while (true) {
      val x: Digits = Digits(5000)
      println(s"**************\nStarting with\n$x")
      val n = x.stoppingTime(true)
      println(s"\nStopping time is: $n")
      if (n < 0)
        exit(1)
    }
  }
}
