package week4

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def +(that: Nat): Nat
  def -(that: Nat): Nat
  def toInt: Int
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new IllegalStateException("Zero.pred")

  override def successor: Nat = new Successor(Zero)

  override def +(that: Nat) = that

  override def -(that: Nat) = that match {
    case Zero => this
    case _ => throw new IllegalArgumentException("Zero - Successor")
  }

  override def toInt = 0

}

case class Successor(pred: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = pred

  override def successor: Nat = new Successor(this)

  override def +(that: Nat) = {
    that match {
      case Zero => this
      case Successor(_) => new Successor(this) + that.predecessor
    }
  }

  override def -(that: Nat) = {
    that match {
      case Zero => this
      case Successor(_) => this.predecessor - that.predecessor
    }
  }

  override def toInt = 1 + pred.toInt

}
