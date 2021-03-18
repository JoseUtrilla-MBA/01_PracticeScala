import scala.annotation.tailrec

object PracticeW1 {

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def insideMethodBalance(chars: List[Char], isCorrectBalance: Boolean, n: Int): Boolean = {
      if (chars.isEmpty) isCorrectBalance
      else if (isCorrectBalance && chars.head == ')') false
      else if (chars.head == '(')
        insideMethodBalance(chars.tail, false, n + 1)
      else if (!isCorrectBalance && chars.head == ')' && n == 1)
        insideMethodBalance(chars.tail, true, n - 1)
      else if (!isCorrectBalance && chars.head == ')' && n > 1)
        insideMethodBalance(chars.tail, false, n - 1)
      else insideMethodBalance(chars.tail, isCorrectBalance, n)
    }

    if (chars.isEmpty) true
    else
      insideMethodBalance(chars, true, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty||money<0) 0
    else if (money == 0) 1
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}

