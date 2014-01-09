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
  val store: Store[Stored], val taintStore: Set[Address], val contextTaint: Set[LineOfCode],
  val stack: Kontinuation) {
  val noResultStore = store - ResultAddress
  val noResultTaintStore = taintStore - ResultAddress
  // TODO must-reach
  val paredContextTaint = contextTaint
  val pass = State(loc.next, env, noResultStore, noResultTaintStore, paredContextTaint, stack)
  val passSet = Set(pass)
  def jump(l: Label): State[Stored] = State(loc.jump(l), env, noResultStore, noResultTaintStore, paredContextTaint, stack)
  def addToContextTaint(ploc: LineOfCode): State[Stored] = State(loc, env, store, taintStore, contextTaint + ploc, stack)
  def next: Set[State[Stored]] = {
    def maybeAlloc(v: Variable): Env =
      if (env isDefinedAt v)
        env
      else
        env + Pair(v, Analyzer.allocator.alloc(v))
    def nextNoContext: Set[State[Stored]] = {
      loc.statement match {
        // Label
        case LabelStatement(l) => passSet

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
        case GotoStatement(l) => Set(jump(l))

        // If
        case IfStatement(condition, l) =>
          val cond = Evaluator.eval(condition, env, store)
          val jumpSet = Set(jump(l))
          val maybeJump: Set[State[Stored]] = if (cond.mayBeNonzero) jumpSet else Set.empty
          val maybePass: Set[State[Stored]] = if (cond.mayBeZero) passSet else Set.empty
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
            (param, exp) <- target.params zip exps
          } yield Pair(newEnv(param), Evaluator.eval(exp, env, store))
          val newStore = noResultStore ++ newValues

          // update the taint store
          val taintedParams = for {
            (param, exp) <- target.params zip exps if (Evaluator.tainted(exp, env, taintStore))
          } yield newEnv(param)
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
          def throwException(loc: LineOfCode, konts: Set[Kontinuation]): Set[State[Stored]] = {
            val sets = for {
              kont <- konts
            } yield loc.findExceptionHandlerTarget match {
              case Some(l) =>
                Set(State(l, env, noResultStore, noResultTaintStore, paredContextTaint, kont))
              case None => kont match {
                case `halt` => throw TopLevelException(e)
                case ConcreteKontinuation(env, ts, contextTaint, kloc, nextAddr) =>
                  throwException(kloc, store(nextAddr))
                case _ => throw BadKontinuationException
              }
            }
            // condense a set of sets into a single set
            sets flatMap { (s: Set[State[Stored]]) => s }
          }
          throwException(loc, Set(stack))

        // Catch (handled statically)
        case CatchDirective(begin, end, handler) => passSet

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
    val returnSet = nextNoContext
    val locSet = returnSet map { _.loc }
    if (locSet.size > 1) returnSet map { _.addToContextTaint(loc) } else returnSet
  }

  def mustReach: Set[LineOfCode] = {
    loc.f.statements(loc.ln) match {
      case l: LabelStatement => pass.mustReach + pass.loc
      case a: AssignmentStatement => pass.mustReach + pass.loc
      case GotoStatement(l) =>
        val target = jump(l)
        target.mustReach + target.loc
      case IfStatement(c, l) =>
        val target = jump(l)
        target.mustReach & pass.mustReach
      case f: FunctionCall => throw NotImplementedException
      case r: ReturnStatement => throw NotImplementedException
      case t: ThrowStatement => throw NotImplementedException
      case c: CatchDirective => pass.mustReach + pass.loc
      case f: FunctionDeclaration => throw NestedFunctionException
      case `FunctionEnd` => throw NotImplementedException
      case m: MoveResult => pass.mustReach + pass.loc
    }
  }
}