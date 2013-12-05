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

import scala.language.postfixOps
import scala.collection.immutable.HashMap

import TypeAliases._
import scala.collection.GenTraversableOnce

case object NotImplementedException extends RuntimeException
case object ImpossibleException extends RuntimeException

abstract class Expression
case class Addition(lhs: Expression, rhs: Expression) extends Expression
case class Multiplication(lhs: Expression, rhs: Expression) extends Expression
case class Comparison(lhs: Expression, rhs: Expression) extends Expression
case class Variable(v: String) extends Expression
case class ConcreteInt(v: Int) extends Value {
  override def +(value: Value): Value = value match {
    case ConcreteInt(i) => ConcreteInt(v + i)
    case si: SignInt => si + this
  }
  override def *(value: Value): Value = value match {
    case ConcreteInt(i) => ConcreteInt(v * i)
    case si: SignInt => si * this
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

  val nzp = new SignInt(false, false, false)
  val nz = new SignInt(false, false, true)
  val np = new SignInt(false, true, false)
  val zp = new SignInt(true, false, false)
  val n = new SignInt(false, true, true)
  val z = new SignInt(true, false, true)
  val p = new SignInt(true, true, false)
}

trait Value extends Expression {
  def +(v: Value): Value
  def *(v: Value): Value
}

trait AbstractValue extends Value {
  def abstractValue(ci: ConcreteInt): AbstractValue
}

case class SignInt(val negative: Boolean, val zero: Boolean, val positive: Boolean) extends AbstractValue {
  override def +(v: Value): Value = v match {
    case ci: ConcreteInt => SignInt.this + abstractValue(ci)
    case SignInt(nN, nZ, nP) => SignInt(nN || negative, (zero && nZ) || (positive && nN) || (negative && nP), nP || positive)
  }
  override def *(v: Value): Value = v match {
    case ci: ConcreteInt => this * abstractValue(ci)
    case SignInt(oN, oZ, oP) =>
      SignInt((oP && negative) || (oN && positive),
        oZ || zero,
        (oP && positive) || (oN && negative))
  }
  def abstractValue(ci: ConcreteInt): SignInt = new SignInt(ci.v < 0, ci.v == 0, ci.v > 0)
}

abstract sealed class Address
case class ConcreteAddress(a: Int) extends Address
// no need for abstract environments because addresses stay the same
case class Label(l: String)
//case class Function(name: String, parameters: List[Variable], body: List[Statement])
case class StackFrame(target: Int, previousEnv: Map[Variable, Address])
case class AbstractStore() extends HashMap[Address, SignInt]

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

abstract sealed class Statement {
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

case class ExceptionHandler(val begin: Int, val end: Int, val code: Label) {
  def contains(line: Int): Boolean = begin < line && end >= line
}
