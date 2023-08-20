
object Question1 extends App{
  val obj1 = new Rational(8,5)
  val obj2 = new Rational(4,7)

  println(obj1.neg)

  val c = obj1.add(obj2)
  println(c)

}

class Rational( a: Int , b: Int){
  require (deno > 0 , "Denominator must not be zero")

  def numb  = a
  def deno  = b

  def add(r: Rational) = new Rational(this.numb*r.deno + this.deno*r.numb ,  this.deno*r.deno)
  def neg = new Rational(-this.numb , this.deno)
  override def toString(): String = numb + "/" + deno

}
