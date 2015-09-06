package net.scivey.scalisp
import scala.collection.mutable.Map
import net.scivey.scalisp.ast._


object Interpreter {
  def isTruthy(t: Term): Boolean = {
    t match {
      case NilVal => false
      case BoolLit(v) => {
        println("boolLit")
        v
      }
      case IntLit(i) => {
        if (i == 0) {
          false
        } else {
          true
        }
      }
      case StrLit(s) => {
        if (s == "") {
          false
        } else {
          true
        }
      }
      case _ => throw new TypeError("Undefined truthiness for term: " + t)
    }
  }
  def evalFuncCall(source: Func, scope: Scope, args: Seq[Term]): Term = {
    var callScope: Scope = source.context match {
      case None => {
        scope
      }
      case Some(aScope) => {
        scope.push(aScope)
      }
    }
    val bindings = source.params.zip(args)
    var mutableMap = Map[Symbol, Term]()
    mutableMap = mutableMap ++ bindings.toMap.toSeq
    callScope = callScope.push(mutableMap)

    var result = evalTerm(source.body, callScope)
    result match {
      case Func(params, body, None) => {
        Func(params, body, Some(callScope))
      }
      case Func(params, body, Some(ownScope)) => {
        Func(params, body, Some(ownScope.push(callScope)))
      }
      case _ => result
    }

  }
  def evalLet(scope: Scope, unEvaledArgs: Seq[Term]): Term = {
    unEvaledArgs.head match {
      case TermList(bindings) => {
        val indices = bindings.indices
        val leftsOne: Seq[Term] = indices.filter {
          i => i % 2 == 0
        }.map {
          i => bindings(i)
        }
        val lefts: Seq[Symbol] = leftsOne.map {
          case (s: Symbol) => s
          case _ => throw new MalformedException("expected symbol")
        }
        val rights = indices.filter {
          i => i % 2 == 1
        }.map {
          i => bindings(i)
        }.map { t => {
            evalTerm(t, scope)
          }
        }
        var mutableMap = Map[Symbol, Term]()
        mutableMap = mutableMap ++ lefts.zip(rights).toMap
        val callScope = new Scope(scope.bindings ++ mutableMap, None)
        evalTerm(unEvaledArgs(1), callScope)
      }
      case _ => throw new MalformedException("Let requires a list of bindings as its first argument.")
    }
  }
  def evalBuiltin(builtin: Builtin, scope: Scope, unEvaledArgs: Seq[Term]): Term = {
    builtin match {
      case Let => evalLet(scope, unEvaledArgs)
      case Print => {
        println(unEvaledArgs)
        val evaled = unEvaledArgs.map { a =>
          evalTerm(a, scope)
        }
        println(evaled(evaled.length - 1))
        NilVal
        // val arg = evalTerm(unEvaledArgs.head, scope)
        // println(arg)
        // NilVal
      }
      case Define => {
        val sym: Symbol = unEvaledArgs.head match {
          case (s: Symbol) => s
          case _ => throw new MalformedException("need a symbol")
        }
        var symVal = unEvaledArgs(1) match {
          case Func(params, body, _) => Func(params, body, Some(scope))
          case x => {
            evalTerm(x, scope)
          }
        }
        scope.bindings(sym) = symVal
        NilVal
      }
      case Do => {
        val evaled = unEvaledArgs.map { a =>
          evalTerm(a, scope)
        }
        evaled(evaled.length - 1)
      }
      case Mul => {
        val evaled = unEvaledArgs.map { a =>
          evalTerm(a, scope)
        }
        Builtins.mul2(evaled(0), evaled(1))
      }
      case Gt => {
        val evaled = unEvaledArgs.map { a =>
          evalTerm(a, scope)
        }
        Builtins.gt2(evaled(0), evaled(1))
      }
      case Lt => {
        val evaled = unEvaledArgs.map { a =>
          evalTerm(a, scope)
        }
        Builtins.lt2(evaled(0), evaled(1))
      }
      case Add => {
        val evaled = unEvaledArgs.map {a =>
          evalTerm(a, scope)
        }
        Builtins.add2(evaled(0), evaled(1))
      }
      case IfExpr => {
        if (isTruthy(evalTerm(unEvaledArgs(0), scope))) {
          evalTerm(unEvaledArgs(1), scope)
        } else {
          if (unEvaledArgs.length >= 3) {
            evalTerm(unEvaledArgs(2), scope)
          } else {
            NilVal
          }
        }
      }
      case _ => throw new MalformedException("not handled yet")
    }
  }
  def evalTerm(source: Term, scope: Scope): Term = {
    source match {
      case NilVal => NilVal
      case (s: Symbol) => scope.get(s)
      case (b: BoolLit) => b
      case (i: IntLit) => i
      case (s: StrLit) => s
      case (f: FloatLit) => f
      case (f: Func) => evalFuncCall(f, scope, Seq())
      case TermList(terms) => {
        terms.head match {
          case (t: TermList) => {
            val first = evalTerm(t, scope)
            evalTerm(TermList(Seq(first) ++ terms.slice(1, terms.length)), scope)
          }
          case (f: Func) => {
            f match {
              case Func(params, body, None) => {
                Func(params, body, Some(scope))
              }
              case Func(params, body, Some(subScope)) => {
                Func(params, body, Some(scope.push(subScope)))
              }
            }
          }
          case (b: Builtin) => {
            evalBuiltin(b, scope, terms.slice(1, terms.length))
          }
          case (s: Symbol) => {
            scope.get(s) match {
              case (f: Func) => {
                val args = terms.slice(1, terms.length).map { t =>
                  evalTerm(t, scope)
                }
                evalFuncCall(f, scope, args)
              }
              case (b: Builtin) => {
                evalBuiltin(b, scope, terms.slice(1, terms.length))
              }
              case _ => {
                throw new TypeError("sexpr must start with 1) a symbol resolving to a function or 2) a builtin.")
              }
            }
          }
          case _ => {
            val results = terms.map { t =>
              evalTerm(t, scope)
            }
            results(results.length - 1)
          }
        }
      }
      case _ => {
        throw new MalformedException("bad!")
      }
    }
  }

  def evaluate(source: Term): Term = {
    val scope = Scope.root
    evalTerm(source, scope)
  }
}