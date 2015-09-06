package net.scivey.scalisp.ast
import scala.collection.mutable.Map
import net.scivey.scalisp.{ScopeError, Builtins, TypeError}

class Scope(
  val bindings: Map[Symbol, Term],
  val parent: Option[Scope]
){
  def push(newBindings: Map[Symbol, Term]): Scope = {
    new Scope(
      bindings ++ newBindings,
      Some(this)
    )
  }
  def push(subScope: Scope): Scope = {
    new Scope(
      bindings ++ subScope.bindings,
      Some(this)
    )
  }
  def get(s: Symbol): Term = {
    if (bindings.contains(s)) {
      return bindings(s)
    }
    parent match {
      case Some(scope) => scope.get(s)
      case None => throw new ScopeError("symbol not in scope: " + s.name)
    }
  }
  override def toString(): String = {
    // "Scope( " + bindings.toString + ")"
    "Scope"
  }
}

object Scope {

  val add2 = BuiltinFunc({ terms: Seq[Term] =>
    Builtins.add2(terms(0), terms(1))
  })

  val mul2 = BuiltinFunc({ terms: Seq[Term] =>
    Builtins.mul2(terms(0), terms(1))
  })

  val gt2 = BuiltinFunc({ terms: Seq[Term] =>
    Builtins.gt2(terms(0), terms(1))
  })

  val lt2 = BuiltinFunc({ terms: Seq[Term] =>
    Builtins.lt2(terms(0), terms(1))
  })

  val builtinPrint = BuiltinFunc({ terms: Seq[Term] =>
    println(terms(0))
    NilVal
  })

  val builtinCar = BuiltinFunc({ terms: Seq[Term] =>
    terms(0) match {
      case TermList(children) => children.head
      case _ => throw new TypeError("car expects a list")
    }
  })

  val builtinCdr = BuiltinFunc({ terms: Seq[Term] =>
    terms(0) match {
      case TermList(children) => TermList(children.tail)
      case _ => throw new TypeError("car expects a list")
    }
  })

  val builtinList = BuiltinFunc({ terms: Seq[Term] =>
    TermList(terms)
  })

  val rootBindings: Map[Symbol, Term] = Map(
    Symbol("define") -> Define,
    Symbol("let") -> Let,
    Symbol("cons") -> Cons,
    Symbol("print") -> builtinPrint,
    Symbol("+") -> add2,
    Symbol("*") -> mul2,
    Symbol(">") -> gt2,
    Symbol("<") -> lt2,
    Symbol("car") -> builtinCar,
    Symbol("cdr") -> builtinCdr,
    Symbol("list") -> builtinList
  )

  def root(): Scope = {
    new Scope(rootBindings, None)
  }
}

abstract class Term

abstract class Literal extends Term
case class IntLit(v: Int) extends Literal {
  override def toString(): String = v.toString
}

case class StrLit(v: String) extends Literal {
  override def toString(): String = "\"" + v + "\""
}

case class FloatLit(v: Float) extends Literal {
  override def toString(): String = v.toString
}

case class BoolLit(v: Boolean) extends Literal {
  override def toString(): String = v.toString
}

case object NilVal extends Literal {
  override def toString(): String = "Nil"
}


case class Symbol(name: String) extends Term {
  override def toString(): String = "Symbol('" + name + "')"
}


case class TermList(terms: Seq[Term]) extends Term

case class Func(params: Seq[Symbol], body: Term, context: Option[Scope]) extends Term

abstract class Builtin extends Term

case object Cons extends Builtin
case object Let extends Builtin
case object Define extends Builtin
case object Print extends Builtin
case object IfExpr extends Builtin

case class BuiltinFunc(func: Seq[Term] => Term) extends Builtin
case object Add extends Builtin
case object Mul extends Builtin
case object Gt extends Builtin
case object Lt extends Builtin
case object Do extends Builtin

