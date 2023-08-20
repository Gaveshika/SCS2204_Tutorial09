
object Question2 extends App{
  val x = new My_Rational(3,4)
  val y = new My_Rational(5,8)
  val z = new My_Rational(2,7)

  val s = x-y-z
  println("x-y-z: "+s)
}

class My_Rational(a: Int , b: Int){
  require(b > 0 , "Denominator must not be zero")
  def numb = a
  def deno = b
  def neg = new My_Rational(-this.numb, this.deno)
  def add(r:My_Rational) = new My_Rational(this.numb*r.deno + this.deno*r.numb ,  this.deno*r.deno)
  def -(r:My_Rational)= this.add(r.neg)

  override def toString(): String = numb + "/" + deno
}
