package week4

import org.scalatest.{FlatSpec, Matchers}
import week4.patmat.Huffman._
import week4.patmat.{Fork, Leaf}

class HuffmanSuite extends FlatSpec with Matchers {


  val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
  val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)


  "weight of a larger tree" should "return 5" in {
    assert(5 == weight(t1))
  }

  "chars of t2 tree" should " must be: a, b, d" in {
    assert(List('a', 'b', 'd') == chars(t2))
  }
  
  "string2chars hello world" should "be as a List of each character that make up this String" in {
    assert(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd') == string2Chars("hello, world"))
  }

  "make ordered leaf list for some frequency table" should "be sorted by weight of leaf" in {
    assert(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)) == makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))
  }

  "method combine" should "take the two first elements of an ordered list, make up a Fork with them and add it again to the list keeping order" in {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)) == combine(leaflist))
  }

  "decode and encode a very short text" should "be identity" in {
    assert("ab".toList == decode(t1, encode(t1)("ab".toList)))
  }

}
