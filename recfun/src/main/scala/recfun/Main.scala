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
    else pascal(c-1, r) * (r - c  + 1)/ c
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = ???

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
