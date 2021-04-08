package week6

import org.scalatest.{FlatSpec, Matchers}

class AnagramsSuite extends FlatSpec with Matchers {

  import Anagrams._

  "wordOccurrences of the word 'abcd'" should "" in {
    assert(List(('a', 1), ('b', 1), ('c', 1), ('d', 1)) == wordOccurrences("abcd"))
  }

  "wordOccurrences of the word 'Robert'" should "return --> List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1))" in {
    assert(List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)) == wordOccurrences("Robert"))
  }

  "sentenceOccurrences: abcd e" should "return --> List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1))" in {
    assert(List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)) == sentenceOccurrences(List("abcd", "e")))
  }

  "dictionaryByOccurrences.get: eat" should "return --> Set('ate', 'eat', 'tea')" in {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet).contains(Set("ate", "eat", "tea")))
  }

  "wordAnagrams married" should "return --> Set('married', 'admirer')" in {
    assert(Set("married", "admirer") == wordAnagrams("married").toSet)
  }

  "wordAnagrams player " should "return --> Set('parley', 'pearly', 'player', 'replay')" in {
    assert(Set("parley", "pearly", "player", "replay") == wordAnagrams("player").toSet)
  }

  "subtract: lard - r" should "return --> List(('a', 1), ('d', 1), ('l', 1))" in {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(lad == subtract(lard, r))
  }


  "combinations: []" should "return --> List(Nil)" in {
    assert(List(Nil) == combinations(Nil))
  }

  "combinations: abba" should "return --> " +
    "List(List(),List(('a', 1)),List(('a', 2)),List(('b', 1)),List(('a', 1), ('b', 1)),List(('a', 2), " +
    "('b', 1)), List(('b', 2)),List(('a', 1), ('b', 2)), List(('a', 2), ('b', 2))) " in {

    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(abbacomb.toSet == combinations(abba).toSet)
  }

  "sentence anagrams: []" should "return --> List(Nil)" in {
    val sentence = List()
    assert(List(Nil) == sentenceAnagrams(sentence))
  }

  "sentence anagrams: Linux rulez" should "return -->  " +
    "List( List('Rex', 'Lin', 'Zulu'), List('nil', 'Zulu', 'Rex'), List('Rex', 'nil', 'Zulu'), " +
    "List('Zulu', 'Rex', 'Lin'), List('null', 'Uzi', 'Rex'), List('Rex', 'Zulu', 'Lin'), " +
    "List('Uzi', 'null', 'Rex'), List('Rex', 'null', 'Uzi'), List('null', 'Rex', 'Uzi'), " +
    "List('Lin', 'Rex', 'Zulu'), List('nil', 'Rex', 'Zulu'), List('Rex', 'Uzi', 'null'), " +
    "List('Rex', 'Zulu', 'nil'), List('Zulu', 'Rex', 'nil'), List('Zulu', 'Lin', 'Rex'), " +
    "List('Lin', 'Zulu', 'Rex'), List('Uzi', 'Rex', 'null'), List('Zulu', 'nil', 'Rex'), " +
    "List('rulez', 'Linux'), List('Linux', 'rulez'))" in {

    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(anas.toSet == sentenceAnagrams(sentence).toSet)
  }

}