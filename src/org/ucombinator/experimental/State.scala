package org.ucombinator.experimental

import TypeAliases._

case object NestedFunctionException extends RuntimeException

case class State[Stored <: Value](val ln: Int, val f: Function, val env: Env,
  val store: Store[Stored], val taintStore: Set[Address], val contextTaint: Set[Pair[Function, Int]],
  val stack: Kontinuation) {
  def next: State[Stored] = {
    def maybeAlloc(v: Variable): Env =
      if (env isDefinedAt v)
        env
      else
        env + Pair(v, Analyzer.allocator.alloc(v))
    val noResultStore = store - ResultAddress
    val noResultTaintStore = taintStore - ResultAddress
    val pass = State(ln + 1, f, env, noResultStore, noResultTaintStore, contextTaint, stack)
    f.statements(ln) match {
      case LabelStatement(l) => pass
      case AssignmentStatement(v, e) => throw NotImplementedException
      case GotoStatement(l) => throw NotImplementedException
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