import PracticeW1._
import org.scalatest._


class TestPracticeScalaW1 extends FlatSpec with Matchers {


  // ------ balance tests -----------------------------------------------------
  "balance: '(if (zero? x) max (/ 1 x))" should "be balanced" in {
    (balance("(if (zero? x) max (/ 1 x))".toList)) == true


  }

  "balance: 'I told him ...' is balanced`" should "be true" in {
    (balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList)) == true

  }
  "`balance: ':-)' is unbalanced`" should "be false" in {
    (balance(":-)".toList)) == false
  }

  "`balance: counting is not enough`" should "be false" in {
    (balance("())(".toList)) == false
  }
  // ------ countChange tests -------------------------------------------------

  "`countChange: example given in instructions`" should "be 3" in {
    (3 == countChange(4, List(1, 2))) == true
  }
  "`countChange: sorted CHF`" should "be 1022" in {
    (1022 == countChange(300, List(5, 10, 20, 50, 100, 200, 500))) == true
  }

  "`countChange: no pennies`" should "be 0" in {
    (0 == countChange(301, List(5, 10, 20, 50, 100, 200, 500))) == true
  }
  "`countChange: unsorted CHF`" should "be 1022" in {
    (1022 == countChange(300, List(500, 5, 50, 100, 20, 200, 10))) == true
  }

  // ------ pascal tests ------------------------------------------------------

  "`pascal: col=0,row=2`" should "be 1" in {
    (1 == pascal(0, 2))
  }

  "`pascal: col=1,row=2`" should "be 2" in {
    (2 == pascal(1, 2))
  }

  "`pascal: col=1,row=3`" should "be 3" in {
    (3 == pascal(1, 3))
  }

}
