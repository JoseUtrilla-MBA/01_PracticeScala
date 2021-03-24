package week3

import org.scalatest.{FlatSpec, Matchers}
import week3.objsets.{Empty, Tweet, TweetSet}

class TweetSetSuite extends FlatSpec with Matchers {

  val set1 = new Empty
  val set2 = set1.incl(new Tweet("a", "a body", 20))
  val set3 = set2.incl(new Tweet("b", "b body", 20))
  val c = new Tweet("c", "c body", 7)
  val d = new Tweet("d", "d body", 9)
  val set4c = set3.incl(c)
  val set4d = set3.incl(d)
  val set5 = set4c.incl(d)


  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  "filter: on empty set" should "be 0" in {
    assert(0 == size(set1.filter(tw => tw.user == "a")))
  }

  "filter: a on set5" should "be 1" in {
    assert(1 == size(set5.filter(tw => tw.user == "a")))
  }
  "filter: twenty on set5" should "be 2" in {
    assert(2 == size(set5.filter(tw => tw.retweets == 20)))
  }
  "union: set4c and set4d" should "be 4" in {
    assert(4 == size(set4c.union(set4d)))
  }
  "union: with empty set1" should "be 4" in {
    assert(4 == size(set5.union(set1)))
  }
  "union: with empty set2" should "be 4" in {
    assert(4 == size(set1.union(set5)))
  }

  "descending: set5" should "" in {
    val trends = set5.descendingByRetweet
    assert(!trends.isEmpty)
    assert(trends.head.user == "a" || trends.head.user == "b")
  }

}
