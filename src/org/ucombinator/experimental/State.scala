package org.ucombinator.experimental

import TypeAliases._

case class State[Stored <: Value](val ln: Int, val f: Function, val env: Env,
  val store: Store[Stored], val taintStore: Set[Address], val contextTaint: Set[Pair[Function, Int]],
  val stack: Kontinuation) {
  def next: State[Stored] = {
    val noResultStore = store + Pair(ResultAddress, store(ResultAddress).noValue)
    val noResultTaintStore = taintStore - ResultAddress
    f.statements(ln) match {
      case LabelStatement(l) => State(ln+1, f, env, noResultStore, noResultTaintStore,
          contextTaint, stack)
      case AssignmentStatement(v, e) => throw NotImplementedException
      case GotoStatement(l) => throw NotImplementedException
      case IfStatement(condition, l) => throw NotImplementedException
      case FunctionCall(fun, exps) => throw NotImplementedException
      case ReturnStatement(e) => throw NotImplementedException
      case ThrowStatement(e) => throw NotImplementedException
      case CatchDirective(begin, end, handler) =>State(ln+1, f, env, noResultStore, noResultTaintStore,
          contextTaint, stack)
      case FunctionDeclaration(name, vars) => throw NotImplementedException
      case FunctionEnd => throw NotImplementedException
      case MoveResult(v) => throw NotImplementedException
    }
  }
}