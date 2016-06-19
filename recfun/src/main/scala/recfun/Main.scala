package recfun

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
    def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    def loop(lparens: Int, cs: List[Char]): Int = {
      if (cs.isEmpty)
        lparens
      else {
        if (cs.head == '(')
          loop(lparens + 1, cs.tail)
        else if (cs.head == ')') {
          if (lparens == 0) -1 else loop(lparens - 1, cs.tail)
        }
        else
          loop(lparens, cs.tail)
      }
    }

    loop(0, chars) == 0
  }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0 || coins.isEmpty) 0
    else if (money == coins.head) 1 + countChange(money, coins.tail)
    else if (money < coins.head) countChange(money, coins.tail)
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
