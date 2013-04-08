package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (x: Int) => x == elem

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (elem: Int) => s(elem) || t(elem)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = (elem: Int) => s(elem) && t(elem)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = (elem: Int) => s(elem) && !t(elem)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (elem: Int) => s(elem) && p(elem)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   *   mapReduce(s, p, true, (x, y) => x && y)
   * 
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a)) p(a) && iter(a + 1)
      else iter(a + 1)
    }
    iter(-bound)
    
  }    

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   * 	mapReduce(s, p, false, (x, y) => x || y)
   */
  def exists(s: Set, p: Int => Boolean): Boolean = ! forall(s, (x) => ! p(x))  

  /*
   *   def mapReduce(s: Set, p: Int => Boolean, outOfBounds: Boolean, reduce: (Boolean, Boolean) => Boolean) = {
    def iter(a: Int): Boolean = {
      if (a > bound) outOfBounds
      else if (s(a)) reduce(p(a), iter(a + 1))
      else iter(a + 1)
    }
    iter(-bound)
  }
   *   
   */

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   * s = {1, 2, 3} = x == 1 || x == 2 || x == 3
   * f = x => x*2
   * map(s, x => x*2) = {2, 4, 6} = x == 1*2 || x == 2*2 || x == 3*2
   * (x) => exist(s, y => f(y) == x)
   *
   *
   */
  def map(s: Set, f: Int => Int): Set = (elem: Int) => exists(s, x => f(x) == elem)

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
