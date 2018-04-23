trait Fraction {
  val num: Int
  val denom: Int
  def *(other: Fraction): Fraction
  def +(other: Fraction): Fraction
  def -(other: Fraction): Fraction
  def /(other: Fraction): Fraction
}
  
trait Loggable {
  def log(timeStamp: Long, msg: String) = println("[" + timeStamp.toString + "]: " + msg)
}

trait Simplifiable {
    def simpl(frac: Fraction) = {
        def gcd(x: Int, y: Int) = {
            var x1 = x  
            var y1 = y
            while(y1 != 0) {
                var r = x1 % y1 
                x1 = y1
                y1 = r
            }
            x1
        }
        var fracGcd = gcd(frac.num, frac.denom)
        Fraction(frac.num / fracGcd, frac.denom / fracGcd)
    } 
}
  
object Fraction {
  // one of the "creational patterns/idioms"
  def apply(num: Int, denom: Int, loggable: Boolean = false, simplifiable: Boolean = true): Fraction =
    if (simplifiable) new SimplifiableImpl(num, denom) else new DefaultImpl(num, denom) 
  
  private class DefaultImpl(val num: Int, val denom: Int) extends Fraction {
    override def *(other: Fraction): Fraction =
      Fraction(this.num * other.num, this.denom * other.denom)
    override def +(other: Fraction): Fraction = 
        Fraction(this.num * other.denom + other.num * this.denom, this.denom * other.denom)
    override def -(other: Fraction): Fraction = 
        Fraction(this.num * other.denom - other.num * this.denom, this.denom * other.denom)
    override def /(other: Fraction): Fraction =
      Fraction(this.num * other.denom, this.denom * other.num)
    override def toString() = num.toString + "/" + denom.toString
  }
   
  private class LoggableImpl(num: Int, denom: Int) extends DefaultImpl(num, denom) with Loggable {
    def timeStamp = System.nanoTime // to keep the example short...
    override def *(other: Fraction): Fraction = {
      log(timeStamp, "multiplying " + this.toString + " by " + other.toString)
      Fraction(this.num * other.num, this.denom * other.denom, true) // super.*(other) is not loggable
    }
    override def +(other: Fraction): Fraction = {
      log(timeStamp, "adding " + this.toString + " to " + other.toString)
      Fraction(this.num * other.denom + other.num * this.denom, this.denom * other.denom, true)
    }
    override def -(other: Fraction): Fraction = {
      log(timeStamp, "subtracting " + this.toString + " from " + other.toString)
      Fraction(this.num * other.denom - other.num * this.denom, this.denom * other.denom, true)
    }
    override def /(other: Fraction): Fraction = {
      log(timeStamp, "dividing " + this.toString + " by " + other.toString)
      Fraction(this.num * other.denom, this.denom * other.num, true)
    }
  }

  private class SimplifiableImpl(num: Int, denom: Int) extends DefaultImpl(num, denom) with Simplifiable {
    override def *(other: Fraction): Fraction = {
      simpl(Fraction(this.num * other.num, this.denom * other.denom, true)) // super.*(other) is not loggable
    }
    override def +(other: Fraction): Fraction = {
      simpl(Fraction(this.num * other.denom + other.num * this.denom, this.denom * other.denom, true))
    }
    override def -(other: Fraction): Fraction = {
      simpl(Fraction(this.num * other.denom - other.num * this.denom, this.denom * other.denom, true))
    }
    override def /(other: Fraction): Fraction = {
      simpl(Fraction(this.num * other.denom, this.denom * other.num, true))
    }
  }
}
  
object Appl {
  def main(agrs: Array[String]) {
    val f1 = Fraction(1, 6)
    val f2 = Fraction(1, 3)
    val f3 = Fraction(1, 19, true)
    val f1plusf2 = f1 + f2
    println(f1.toString + " + " + f2.toString + " = " + f1plusf2)
    //println(f3.toString + " * " + f2.toString + " * " + f1.toString + " = " + f3 * f2 * f1)
  }
}