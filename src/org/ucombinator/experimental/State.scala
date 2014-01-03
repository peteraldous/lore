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

import scala.Option.option2Iterable
import scala.language.postfixOps
import scala.reflect.ClassTag

import Env.Env

case object NestedFunctionException extends RuntimeException
case object NoSuchFunctionException extends RuntimeException
case object ArityMismatchException extends RuntimeException
case object BadKontinuationException extends RuntimeException
case class TopLevelException(e: Expression) extends RuntimeException
case object NotImplementedException extends RuntimeException
case object ImpossibleException extends RuntimeException

case class State[Stored <: Value: ClassTag](val loc: LineOfCode, val env: Env,
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
    // TODO must-reach
    // sub-TODO create a call graph - each function must be associated with all LOCs that may invoke it
    val paredContextTaint = contextTaint
    val pass = Set(State(loc.next, env, noResultStore, noResultTaintStore, paredContextTaint, stack))
    loc.statement match {
      // Label
      case LabelStatement(l) => pass

      // Assignment
      case AssignmentStatement(v, e) =>
        val newEnv = maybeAlloc(v)
        val value = Evaluator.eval[Stored](e, env, store)
        val newStore = noResultStore + Pair(newEnv(v), value)
        val newTaintStore = if (!(contextTaint isEmpty) || Evaluator.tainted(e, env, taintStore)) {
          taintStore + newEnv(v)
        } else {
          taintStore
        }
        Set(State(loc.next, newEnv, newStore, newTaintStore, paredContextTaint, stack))

      // Goto
      case GotoStatement(l) =>
        Set(State(loc.jump(l), env, noResultStore, noResultTaintStore, paredContextTaint, stack))

      // If
      case IfStatement(condition, l) =>
        val cond = Evaluator.eval(condition, env, store)
        val jump = Set(State(loc.jump(l), env, noResultStore,
          noResultTaintStore, paredContextTaint, stack))
        val maybeJump: Set[State[Stored]] = if (cond.mayBeNonzero) jump else Set.empty
        val maybePass: Set[State[Stored]] = if (cond.mayBeZero) pass else Set.empty
        maybeJump | maybePass

      // Function call
      case FunctionCall(fun, exps) =>
        val target: Function = Analyzer.functionTable.get(fun) match {
          case Some(f) => f
          case None => throw NoSuchFunctionException
        }
        if (target.params.length != exps.length) throw ArityMismatchException

        // Observe that, as the functions are static and flat, each new environment contains
        // exactly the bindings specified in the function's parameter list.
        val newEnv = Env(target.params map {
          (param: Variable) => Pair(param, Analyzer.allocator.alloc(param))
        })

        // update the store
        val newValues = for {
          param <- target.params
          exp <- exps
        } yield Pair(newEnv(param), Evaluator.eval(exp, env, store))
        val newStore = noResultStore ++ newValues

        // update the taint store
        val taintedOptionParams: List[Option[Address]] = (for {
          param <- target.params
          exp <- exps
        } yield if (Evaluator.tainted(exp, env, taintStore)) Some(newEnv(param)) else None)
        val taintedParams = taintedOptionParams flatMap { (oa: Option[Address]) => oa }
        val newTaintStore = noResultTaintStore ++ taintedParams

        // Kontinuation addresses, in this formulation, are based on call site
        val kontAddr = Analyzer.kontAllocator.kalloc(loc)
        val newNewStore = newStore + Pair(kontAddr, stack)
        val newStack = ConcreteKontinuation(env, noResultTaintStore, paredContextTaint, loc.next, kontAddr)
        Set(State(target.init, newEnv, newStore, newTaintStore, paredContextTaint, stack))

      // Return
      case ReturnStatement(e) => stack match {
        case ck: ConcreteKontinuation[Stored] =>
          val result = Evaluator.eval(e, env, store)
          val resultStore = noResultStore + Pair(ResultAddress, result)
          val resultTaintStore = if (Evaluator.tainted(e, env, taintStore)) {
            noResultTaintStore + ResultAddress
          } else {
            noResultTaintStore
          }
          ck.call(resultStore, resultTaintStore)
        case `halt` => Set.empty
        case _ => throw BadKontinuationException
      }

      // Throw
      case ThrowStatement(e) =>
        def throwException(loc: LineOfCode): Set[State[Stored]] = {
          loc.findExceptionHandlerTarget match {
            case Some(l) =>
              Set(State(l, env, noResultStore, noResultTaintStore, paredContextTaint, stack))
            case None => stack match {
              case `halt` => throw TopLevelException(e)
              case ConcreteKontinuation(env, ts, contextTaint, kloc, nextAddr) =>
                throwException(kloc)
              case _ => throw BadKontinuationException
            }
          }
        }
        throwException(loc)

      // Catch (handled statically)
      case CatchDirective(begin, end, handler) => pass

      // Function declaration (should never occur inside a function)
      case FunctionDeclaration(name, vars) => throw NestedFunctionException

      // Function end
      case FunctionEnd => stack match {
        case ck: ConcreteKontinuation[Stored] => ck.call(noResultStore, noResultTaintStore)
        case `halt` => Set.empty
        case _ => throw BadKontinuationException
      }

      // Move result (get the return value from the previous statement)
      case MoveResult(v) =>
        val newEnv = maybeAlloc(v)
        val newStore = noResultStore + Pair(newEnv(v), store(ResultAddress))
        val newTaintStore =
          if (taintStore contains ResultAddress)
            noResultTaintStore + newEnv(v)
          else
            noResultTaintStore
        Set(State(loc.next, newEnv, newStore, newTaintStore, paredContextTaint, stack))
    }
  }
}