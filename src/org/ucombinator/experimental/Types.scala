/*
    Implicit Flows: a prototype taint tracking system for implicit flows
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

import scala.language.postfixOps
import scala.collection.immutable.HashMap

import TypeAliases._
import scala.collection.GenTraversableOnce

// TODO: make Expressions and Values and so on implement an interface for extensibility to the value system

abstract class Expression
case class Addition(lhs: Expression, rhs: Expression) extends Expression
case class Multiplication(lhs: Expression, rhs: Expression) extends Expression
case class Comparison(lhs: Expression, rhs: Expression) extends Expression
case class Variable(v: String) extends Expression
case class ConcreteInt(v: Int) extends Value {
  def abstractMe: AbstractInt = v match {
    case 0 => z
    case i if i < 0 => n
    case i if i > 0 => p
  }
  override def +(value: Value): Value = value match {
    case ConcreteInt(i) => ConcreteInt(v+i)
    case ai: AbstractInt => ai+abstractMe
  }
}

object TypeAliases {
  type Env = Map[Variable, Address]
  /*
  case class Store(val values: Map[Address, Value], val stack: Map[Address, Kontinuation]) {
    def apply(): Store = Store(values.empty, stack.empty)
    def apply(a: Address) = values(a)
    def +(a: Address, v: Value): Store = new Store(values + Pair(a, v), stack)
    def +(a: Address, k: Kontinuation): Store = new Store(values, stack + Pair(a, k))
    def +(p: Pair[Address, Value]): Store = new Store(values + p, stack)
    def ++(pairs: Pair[Address, Value]*): Store = new Store(values ++ pairs, stack)
    def ++(l: List[Pair[Address, Value]]): Store = new Store(values ++ l, stack)
  }
  */
  object Env {
    def apply(): Env = Map.empty
    def apply(v: Variable, a: Address): Env = Map(v -> a)
    def apply(pairs: Pair[Variable, Address]*): Env = Map(pairs: _*)
    def apply(l: List[Pair[Variable, Address]]): Env = Map(l: _*)
  }
  /*
  object Store {
    def apply(): Store = new Store(Map.empty, Map.empty)
    def apply(a: Address, v: Value): Store = new Store(Map(a -> v), Map.empty)
    def apply(pairs: Pair[Address, Value]*): Store = new Store(Map(pairs: _*), Map.empty)
    def apply(l: List[Pair[Address, Value]]): Store = new Store(Map(l: _*), Map.empty)
  }
  */
//  val noFunction = Function("noFunction", List.empty, List.empty)
  case class ConcreteResult(val value: Value, val tainted: Boolean)
  type Result = Option[ConcreteResult]
  object Result {
    def apply(v: Value, t: Boolean): Result = Some(ConcreteResult(v, t))
  }
}

case class Address(a: Int)
// no need for abstract environments because addresses stay the same
case class Label(l: String)
//case class Function(name: String, parameters: List[Variable], body: List[Statement])
case class StackFrame(target: Int, previousEnv: Map[Variable, Address])
case class AbstractStore() extends HashMap[Address, AbstractInt]

/*
class Kontinuation
case class ConcreteKontinuation(val env: Env, val taintedVars: Set[Variable], val contextTaint: Set[Int], val f: Function, val ln: Int, val nextAddr: Address) extends Kontinuation {
  def call(p: ConcreteProgram, s: Store, result: Result = None): ConcreteState =
    ConcreteState(p, f, ln, env, s, s.stack(nextAddr), result, taintedVars, contextTaint)
}
object halt extends Kontinuation
*/

// states are (ln, env, store)
// configurations are (state, stack summary)

trait Value extends Expression {
  def +(v: Value): Value
}

abstract class AbstractInt() extends Value

case object NotImplementedException extends RuntimeException

trait AbstractNegative extends AbstractInt
trait AbstractZero extends AbstractInt
trait AbstractPositive extends AbstractInt

case object nzp extends AbstractInt with AbstractNegative with AbstractZero with AbstractPositive {
  override def +(v: Value): Value = this
}
case object nz extends AbstractInt with AbstractNegative with AbstractZero {
  override def +(v: Value): Value = throw NotImplementedException
}
case object np extends AbstractInt with AbstractNegative with AbstractPositive {
  override def +(v: Value): Value = throw NotImplementedException
}
case object zp extends AbstractInt with AbstractZero with AbstractPositive {
  override def +(v: Value): Value = throw NotImplementedException
}
case object n extends AbstractInt with AbstractNegative {
  override def +(v: Value): Value = throw NotImplementedException
}
case object z extends AbstractInt with AbstractZero {
  override def +(v: Value): Value = throw NotImplementedException
}
case object p extends AbstractInt with AbstractPositive {
  override def +(v: Value): Value = throw NotImplementedException
}

object AbstractValues {
  val positive = Set[AbstractInt](nzp, np, zp, p)
  val zero = Set[AbstractInt](nzp, nz, zp, z)
  val negative = Set[AbstractInt](nzp, nz, np, n)
  val all = positive | zero | negative
}

abstract class Statement {
  def isEndOfFunction: Boolean
}
case class LabelStatement(val l: Label) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(Label " + l + ")\""
}
case class AssignmentStatement(val v: Variable, val e: Expression) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(:= " + v + e + ")\""
}
case class GotoStatement(val l: Label) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(Goto " + l + ")\""
}
case class IfStatement(val condition: Expression, val l: Label) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(If " + condition + l + ")\""
}
case class FunctionCall(val fun: String, val exps: List[Expression]) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(" + fun + " " + exps + ")\""
}
case class ReturnStatement(val e: Expression) extends Statement {
  override def isEndOfFunction: Boolean = true
  override def toString: String = super.toString + "\"(Return " + e + ")\""
}
case class ThrowStatement(val e: Expression) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(throw " + e + ")\""
}
case class CatchDirective(val begin: Label, val end: Label, val handler: Label) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(catch " + begin + " " + end + " " + handler + ")\""
}
case class FunctionDeclaration(val name: String, val vars: List[Variable]) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(fun " + name + " " + vars mkString (" ") + ")\""
}
case object FunctionEnd extends Statement {
  override def isEndOfFunction: Boolean = true
  override def toString: String = super.toString + "\"(endfun)\""
}
case class MoveResult(val v: Variable) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(result " + v + ")\""
}

case class Handler(val begin: Int, val end: Int, val code: Label) {
  def contains(line: Int): Boolean = begin < line && end >= line
}
