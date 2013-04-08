package greeter

object Sheet {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }                                               //> sum: (f: Int => Int)(a: Int, b: Int)Int

  sum(x => x)(1, 5)                               //> res0: Int = 15

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) * acc)
    }
    loop(a, 1)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int

  product(x => x)(1, 5)                           //> res1: Int = 120

  def factorial(b: Int): Int = {
    product(x => x)(1, b)
  }                                               //> factorial: (b: Int)Int

  factorial(1)                                    //> res2: Int = 1
  factorial(2)                                    //> res3: Int = 2
  factorial(3)                                    //> res4: Int = 6
  factorial(4)                                    //> res5: Int = 24

  def doCurry(oper: (Int, Int) => Int, f: Int => Int, start: Int)(a: Int, b: Int) = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, oper(f(a), acc))
    }
    loop(a, 1)
  }                                               //> doCurry: (oper: (Int, Int) => Int, f: Int => Int, start: Int)(a: Int, b: Int
                                                  //| )Int
  doCurry((x, y) => x * y, x => x, 1)(1, 4)       //> res6: Int = 24

  val x = new Rational(1, 3)                      //> x  : greeter.Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : greeter.Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : greeter.Rational = 3/2
  y + z                                           //> res7: greeter.Rational = 31/14
  y + y                                           //> res8: greeter.Rational = 10/7
  y - z                                           //> res9: greeter.Rational = -11/14
  x - y - z                                       //> res10: greeter.Rational = -79/42
  x < y                                           //> res11: Boolean = true
  x max y                                         //> res12: greeter.Rational = 5/7
  new Rational(2)                                 //> res13: greeter.Rational = 2/1
}

class Rational(x: Int, y: Int) {
	require(y != 0, "denominator must be non-zero")
	
	def this(x: Int) = this(x,1)
	
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer = x
  def denom = y
  
  def < (that:Rational) = numer * that.denom < that.numer * denom
  
  def max(that:Rational) = if (this < that) that else this
  
  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)
  def - (that: Rational) = this + -that

  def unary_- = new Rational(-numer, denom)

  override def toString = (numer/g)  + "/" + (denom/g)
}