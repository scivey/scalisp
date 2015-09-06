package net.scivey.scalisp
import scala.collection.mutable.Map
import net.scivey.scalisp.ast._


object Interpreter {
  def evalFuncCall(source: Func, scope: Scope, args: Seq[Term]): Term = {
    val callScope: Scope = (source.context match {
      case None => {
        scope
      }
      case Some(aScope) => {
        scope.push(aScope)
      }
    }).push(
      Map[Symbol, Term]() ++ source.params.zip(args).toMap.toSeq
    )

    evalTerm(source.body, callScope) match {
      case Func(params, body, aScope) => {
        aScope match {
          case Some(subScope) => Func(params, body, Some(subScope.push(callScope)))
          case None => Func(params, body, Some(callScope))
        }
      }
      case x => x
    }
  }

  def evalLet(scope: Scope, unEvaledArgs: Seq[Term]): Term = {
    unEvaledArgs.head match {
      case TermList(bindings) => {

        val bindSyms: Seq[Symbol] = bindings.indices.filter {
          i => i % 2 == 0
        }.map {
          i => bindings(i)
        }.map {
          case (s: Symbol) => s
          case _ => throw new MalformedException("expected symbol")
        }

        val bindVals = bindings.indices.filter {
          i => i % 2 == 1
        }.map {
          i => bindings(i)
        }.map { t =>
          evalTerm(t, scope)
        }

        val callScope = scope.push(
          Map[Symbol, Term]() ++ bindSyms.zip(bindVals).toMap
        )
        evalTerm(unEvaledArgs(1), callScope)

      }
      case _ => throw new MalformedException("Let requires a list of bindings as its first argument.")
    }
  }

  def evalBuiltin(builtin: Builtin, scope: Scope, unEvaledArgs: Seq[Term]): Term = {
    builtin match {
      case Let => evalLet(scope, unEvaledArgs)
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
      case BuiltinFunc(func) => {
        val evaled = unEvaledArgs.map { a =>
          evalTerm(a, scope)
        }
        func(evaled)
      }
      case IfExpr => {
        if (BuiltinHelpers.isTruthy(evalTerm(unEvaledArgs(0), scope))) {
          evalTerm(unEvaledArgs(1), scope)
        } else {
          if (unEvaledArgs.length >= 3) {
            evalTerm(unEvaledArgs(2), scope)
          } else {
            NilVal
          }
        }
      }
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
            val remaining = terms.slice(1, terms.length)
            first match {
              case (f: Func) => {
                evalFuncCall(f, scope, remaining)
              }
              case x => {
                evalTerm(TermList(Seq(x) ++ remaining), scope)
              }
            }
          }
          case Func(params, body, subScope) => {
            subScope match {
              case Some(s) => Func(params, body, Some(scope.push(s)))
              case None    => Func(params, body, Some(scope))
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
    evalTerm(source, Scope.root)
  }
}