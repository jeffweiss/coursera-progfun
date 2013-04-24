package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
  
  test("times") {
    val list = 'a' :: 'b' :: 'a' :: 'c' :: 'b' :: 'a' :: 'a' :: Nil
    val result = times(list)
    assert(result(result.indexWhere(p => p._1 == 'a'))._2 === 4)
    assert(result(result.indexWhere(p => p._1 == 'b'))._2 === 2)
    assert(result(result.indexWhere(p => p._1 == 'c'))._2 === 1)
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("decoded secret") {
    assert(decodedSecret === "huffmanestcool".toList)
  }
  
  test("combine singleton") {
    assert(combine(List(Leaf('a', 1))) === List(Leaf('a', 1)))
  }
  
  test("combine nil") {
    assert(combine(Nil) === Nil)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val t = createCodeTree("qwertyuioplkjhgfdsazxcvbnmaaaaabbbh".toList)
      assert(decode(t, encode(t)("abacccdffaaaffeh".toList)) === "abacccdffaaaffeh".toList)
    }
  }
  
  test("code table") {
    new TestTrees {
      val t = createCodeTree("qwertyuioplkjhgfdsazxcvbnmaaaaabbbh".toList)
      assert(decode(t, quickEncode(t)("abacccdffaaaffeh".toList)) === "abacccdffaaaffeh".toList)
    }    
  }
}
