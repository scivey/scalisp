package net.scivey.scalisp

import net.scivey.scalisp.ast._

object Builtins {
  def add2(left: Term, right: Term): Term = {
    (left, right) match {
      case (IntLit(x), IntLit(y)) => IntLit(x + y)
      case (StrLit(x), IntLit(y)) => StrLit(x + y.toString)
      case (StrLit(x), FloatLit(y)) => StrLit(x + y.toString)
      case (IntLit(_), StrLit(_)) => throw new TypeError("int + str = bad")
      case (FloatLit(_), StrLit(_)) => throw new TypeError("float + str = bad")
      case (IntLit(x), FloatLit(f)) => FloatLit(x + f)
      case (FloatLit(f), IntLit(x)) => FloatLit(x + f)
      case (_, _) => throw new TypeError("undefined additon operation")
    }
  }
  def mul2(left: Term, right: Term): Term = {
    (left, right) match {
      case (IntLit(x), IntLit(y)) => IntLit(x * y)
      case (FloatLit(x), FloatLit(y)) => FloatLit(x * y)
      case (IntLit(x), FloatLit(y)) => FloatLit( x * y )
      case (FloatLit(x), IntLit(y)) => FloatLit( x * y )
      case (StrLit(_), _) => throw new TypeError("multiplication by string")
      case (_, StrLit(_)) => throw new TypeError("multiplication by string")
      case (_, _) => throw new TypeError("undefined multiplication operation")
    }
  }
  def gt2(left: Term, right: Term): Term = {
    val boolVal = (left, right) match {
      case (IntLit(x), IntLit(y)) => x > y
      case (IntLit(x), FloatLit(y)) => x > y
      case (FloatLit(x), FloatLit(y)) => x > y
      case (FloatLit(x), IntLit(y)) => x > y
      case (StrLit(x), StrLit(y)) => x > y
      case (t1, t2) => throw new TypeError("undefined > comparison: " + t1 + t2)
    }
    BoolLit(boolVal)
  }
  def lt2(left: Term, right: Term): Term = {
    val boolVal = (left, right) match {
      case (IntLit(x), IntLit(y)) => x < y
      case (IntLit(x), FloatLit(y)) => x < y
      case (FloatLit(x), FloatLit(y)) => x < y
      case (FloatLit(x), IntLit(y)) => x < y
      case (StrLit(x), StrLit(y)) => x < y
      case (t1, t2) => throw new TypeError("undefined > comparison: " + t1 + t2)
    }
    BoolLit(boolVal)
  }
}
