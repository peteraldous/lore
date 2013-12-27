/*
    Lore: a prototype taint tracking system for implicit flows
    Copyright (C) 2013   Petey Aldous <petey.aldous@utah.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

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