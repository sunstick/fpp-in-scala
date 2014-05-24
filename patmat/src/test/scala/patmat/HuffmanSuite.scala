package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t1) === List('a', 'b'))
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a more complex text") {
    val sentence = "to be or not to be; whatever; let's make more stuff so the encoded tree is more complex"
    val codeTree = createCodeTree(sentence.toList)

    sentence.split(" ") foreach {
      word => assert(decode(codeTree, encode(codeTree)(word.toList)) === word.toList)
    }

    sentence.split("a") foreach {
      word => assert(decode(codeTree, encode(codeTree)(word.toList)) === word.toList)
    }
  }

  test("codeBits find") {
    val table = Map(
      ' ' -> List(1, 1, 1),
      'a' -> List(0, 1, 0),
      'e' -> List(0, 0, 0),
      'f' -> List(1, 1, 0, 1),
      'h' -> List(1, 0, 1, 0),
      'i' -> List(1, 0, 0, 0),
      'm' -> List(0, 1, 1, 1),
      'n' -> List(0, 0, 1, 0),
      's' -> List(1, 0, 1, 1),
      't' -> List(0, 1, 1, 0),
      'l' -> List(1, 1, 0, 0, 1),
      'o' -> List(0, 0, 1, 1, 0),
      'p' -> List(1, 0, 0, 1, 1),
      'r' -> List(1, 1, 0, 0, 0),
      'u' -> List(0, 0, 1, 1, 1),
      'x' -> List(1, 0, 0, 0, 1)
    ).toList

    assert(codeBits(table)('l') === List(1, 1, 0, 0, 1))

  }

  test("simple convert CodeTree to CodeTable") {
    new TestTrees {
      assert(convert(t1) === List(('a', List(0)), ('b', List(1))))
      assert(convert(t2) === List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
    }
  }

  test("encode and quickEncode should be identity") {
    val sentence = "another long list of your favourite chars"
    val tree = createCodeTree(sentence.toList)

    sentence.split(" ") foreach {
      word => assert(encode(tree)(word.toList) === quickEncode(tree)(word.toList))
    }

    sentence.split("E") foreach {
      word => assert(encode(tree)(word.toList) === quickEncode(tree)(word.toList))
    }

    sentence.split("l") foreach {
      word => assert(encode(tree)(word.toList) === quickEncode(tree)(word.toList))
    }

    sentence.split("e") foreach {
      word => assert(encode(tree)(word.toList) === quickEncode(tree)(word.toList))
    }
  }
}
