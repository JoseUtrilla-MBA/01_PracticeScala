package week4

import org.scalatest.{FlatSpec, Matchers}
import week4.patmat.{Fork, Huffman, Leaf}

class HuffmanSuite extends FlatSpec with Matchers {

  import Huffman._

  val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
  val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)


  "weight of a larger tree " should "" in {
    assert(5 == weight(t1))
  }

  "chars of a larger tree" should "" in {
    assert(List('a', 'b', 'd') == chars(t2))
  }
  "string2chars hello world" should "" in {
    assert(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd') == string2Chars("hello, world"))
  }

  "make ordered leaf list for some frequency table" should "" in {
    assert(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)) == makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))
  }

  "combine of some leaf list " should "" in {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)) == combine(leaflist))
  }

  "decode and encode a very short text should be identity " should "" in {
    assert("ab".toList == decode(t1, encode(t1)("ab".toList)))
  }

}
