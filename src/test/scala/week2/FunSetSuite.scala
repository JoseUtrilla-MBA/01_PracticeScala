package week2


import org.scalatest.{FlatSpec, Matchers}
import week2.funsets.FunSets._

class FunSetSuite extends FlatSpec with Matchers {



  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s4 = union(union(union(union(union(s1, s3), singletonSet(4)), singletonSet(5)), singletonSet(7)), singletonSet(1000))

  "if contains is implemented " should " be true " in {
    contains(x => true, 100)
  }


  "singleton set " should " contains an integer element" in {
    assert(contains(s1, 1), "Singleton")
  }


  "union " should " contains all elements of each set" in {
    val s = union(s1, s2)
    assert(contains(s, 1), "Union 1")
    assert(contains(s, 2), "Union 2")
    assert(!contains(s, 3), "Union 3")
  }

  "intersection " should " contains all elements the elements that each set shares" in {
    val s = intersect(union(s1, s2), union(s2, s3))
    assert(!contains(s, 1), "intersection 1")
    assert(contains(s, 2), "intersection 2")
    assert(!contains(s, 3), "intersection 3")
  }

  "difference between two sets 'a' and 'b' " should " contains all elements of 'a' that are not in 'b'" in {
    val s = diff(union(s1, s2), union(s2, s3))
    assert(contains(s, 1), "diff 1 --> diff(union(s1, s2), union(s2, s3))")
    assert(!contains(s, 2), "diff 2 --> diff(union(s1, s2), union(s2, s3))")
    assert(!contains(s, 3), "diff 3 --> diff(union(s1, s2), union(s2, s3))")
  }

  "for all elements of a set," should " tel me if they are:" in {
    val x = forall(union(s1, s2), x => x > 0)
    assert(x == true, "for all the numbers included in the set are greater than zero --> forall(union(s1, s2), x => x > 0)")

    val y = forall(x => (x % 2) == 0, x => x > 0)
    assert(y == false, "for all even numbers are greater than zero --> forall(x => (x % 2) == 0, x => x > 0)")
  }

  "" should " be some negative number in the even numbers" in {
    val x = exists(x => (x % 2) == 0, x => x > 0)
    assert(x == true, "exists(x => (x % 2) == 0, x => x < 0)")
  }

  "applying this filter, set 'a' only " should "contain the element with value = 1" in {
    val s = filter(union(s1, s2), x => x == 1)
    val x = forall(s,x => x == 1)
    assert(x == true, "filter(union(s1, s2), x => x == 1)")
  }
}




