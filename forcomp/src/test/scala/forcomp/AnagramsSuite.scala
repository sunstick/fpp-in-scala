package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("wordOccurrences: bRoter") {
    assert(wordOccurrences("bRoter") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }



  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: abc d e") {
    assert(sentenceOccurrences(List("abc", "d", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: abc cad e") {
    assert(sentenceOccurrences(List("abc", "cad", "e")) === List(('a', 2), ('b', 1), ('c', 2), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: to be or not to be") {
    assert(sentenceOccurrences("to" :: "be" :: "or" :: "not" :: "to" :: "be" :: Nil) === List(('b', 2), ('e', 2), ('n', 1), ('o', 4), ('r', 1), ('t', 3)))
  }



  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }



  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }



  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: lard - lard") {
    val lard1 = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val lard2 = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    assert(subtract(lard1, lard2) === Nil)
  }

  test("subtract: lllarrrd - r") {
    val lllarrrd = List(('a', 1), ('d', 2), ('l', 3), ('r', 3))
    val r = List(('r', 1))
    val lllarrd = List(('a', 1), ('d', 2), ('l', 3), ('r', 2))
    assert(subtract(lllarrrd, r) === lllarrd)
  }



  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: aa") {
    val aa = List(('a', 2))
    val aacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2))
    )
    assert(combinations(aa).toSet === aacomb.toSet)
  }

  test("combinations: abba") {
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
    assert(combinations(abba).toSet === abbacomb.toSet)
  }



  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
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
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  test("sentence anagrams: Yes man") {
    val sentence = List("Yes", "man")
    val en = "en"
    val as = "as"
    val my = "my"
    val man = "man"
    val yes = "yes"
    val men = "men"
    val say = "say"
    val sane = "sane"
    val Sean = "Sean"
    val anas = List(
      List(en, as, my),
      List(en, my, as),
      List(man, yes),
      List(men, say),
      List(as, en, my),
      List(as, my, en),
      List(sane, my),
      List(Sean, my),
      List(my, en, as),
      List(my, as, en),
      List(my, sane),
      List(my, Sean),
      List(say, men),
      List(yes, man)
    )

    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }


}
