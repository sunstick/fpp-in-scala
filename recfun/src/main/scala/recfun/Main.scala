package recfun

import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (r == 0 || c == 0 || r == c) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def go(chs: List[Char], leftTotal: Int): Boolean = {
      if (chs.isEmpty) leftTotal == 0
      else {
        if (chs.head == '(') go(chs.tail, leftTotal + 1)
        else if (chs.head == ')') {
          if (leftTotal > 0) go(chs.tail, leftTotal - 1)
          else false
        } else go(chs.tail, leftTotal)
      }
    }

    go(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (coins.isEmpty) 0
    else {
      val total =
        for (i <- 0 to (money / coins.head)) yield countChange(money - i * coins.head, coins.tail)
      total.sum
    }

}
