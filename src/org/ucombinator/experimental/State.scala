package org.ucombinator.experimental

import TypeAliases._
import scala.reflect.ClassTag

case object NestedFunctionException extends RuntimeException
case object NoSuchLabelException extends RuntimeException

case class State[Stored <: Value : ClassTag](val ln: Int, val f: Function, val env: Env,
  val store: Store[Stored], val taintStore: Set[Address], val contextTaint: Set[Pair[Function, Int]],
  val stack: Kontinuation) {
  def next: Set[State[Stored]] = {
    def maybeAlloc(v: Variable): Env =
      if (env isDefinedAt v)
        env
      else
        env + Pair(v, Analyzer.allocator.alloc(v))
    val noResultStore = store - ResultAddress
    val noResultTaintStore = taintStore - ResultAddress
    val pass = Set(State(ln + 1, f, env, noResultStore, noResultTaintStore, contextTaint, stack))
    f.statements(ln) match {
      case LabelStatement(l) => pass
      case AssignmentStatement(v, e) =>
        val newEnv = maybeAlloc(v)
        val value = Evaluator.eval[Stored](e, env, store)
        val newStore = noResultStore + Pair(newEnv(v), value)
        // TODO taint
        Set(State(ln + 1, f, newEnv, newStore, taintStore, contextTaint, stack))
      case GotoStatement(l) =>
        val target = f.labelTable.get(l) match {
          case Some(i) => i
          case None => throw NoSuchLabelException
        }
        Set(State(target, f, env, noResultStore, noResultTaintStore, contextTaint, stack))
      case IfStatement(condition, l) => throw NotImplementedException
      case FunctionCall(fun, exps) => throw NotImplementedException
      case ReturnStatement(e) => throw NotImplementedException
      case ThrowStatement(e) => throw NotImplementedException
      case CatchDirective(begin, end, handler) => pass
      case FunctionDeclaration(name, vars) => throw NestedFunctionException
      case FunctionEnd => throw NotImplementedException
      case MoveResult(v) =>
        val newEnv = maybeAlloc(v)
        val newStore = noResultStore + Pair(newEnv(v), store(ResultAddress))
        val newTaintStore =
          if (taintStore contains ResultAddress)
            noResultTaintStore + newEnv(v)
          else
            noResultTaintStore
        State(ln + 1, f, newEnv, newStore, newTaintStore, contextTaint, stack)
        throw NotImplementedException
    }
  }
}