package recfun
import common._

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
   * Exercise 1 - We can calculate a value on a row
   * by doing taking the number immediately to the left
   * multiplying it by (the row + 1 - column) and then
   * dividing by the column
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else pascal(c - 1, r) * (r - c + 1) / c
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def scan(chars: List[Char], count: Int): Boolean = {
      if (count < 0) false
      else if (chars.isEmpty) count == 0
      else {
        val cur = chars.head
        val change = if (cur == '(') 1 else if (cur == ')') -1 else 0
        scan(chars.tail, count + change)
      }
    }

    scan(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def subCount(money: Int, coins: List[Int]): Int = {
      if (money < 0) 0
      else if (money == 0) 1
      else if (coins.isEmpty) 0
      else {
        subCount(money - coins.head, coins) + subCount(money, coins.tail)
      }

    }
    if (money <= 0) 0
    else subCount(money, coins)
  }
}
