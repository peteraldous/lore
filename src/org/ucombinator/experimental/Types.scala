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

// TODO matches are non-exhaustive now that no-value objects have been introduced.

package org.ucombinator.experimental

import scala.language.postfixOps
import scala.collection.immutable.HashMap
import TypeAliases._
import scala.collection.GenTraversableOnce
import scala.reflect.ClassTag

case object NotImplementedException extends RuntimeException
case object ImpossibleException extends RuntimeException

abstract class Expression
case class Addition(lhs: Expression, rhs: Expression) extends Expression
case class Multiplication(lhs: Expression, rhs: Expression) extends Expression
case class Comparison(lhs: Expression, rhs: Expression) extends Expression
case class Variable(v: String) extends Expression

trait Value extends Expression with Storable {
  def +(v: Value): Value
  def *(v: Value): Value
  def ==(v: Value): Value
  def mayBeZero: Boolean
  def mayBeNonzero: Boolean
}
case class ConcreteInt(v: Int) extends Value {
  override def +(value: Value): Value = value match {
    case ConcreteInt(i) => ConcreteInt(v + i)
    case _ => value + this
  }
  override def *(value: Value): Value = value match {
    case ConcreteInt(i) => ConcreteInt(v * i)
    case _ => value * this
  }
  override def ==(value: Value): Value = value match {
    case ConcreteInt(i) if i == v => ConcreteInt(1)
    case ConcreteInt(i) if i != v => ConcreteInt(0)
    case _ => value == this
  }
  override def mayBeZero: Boolean = v == 0
  override def mayBeNonzero: Boolean = v != 0
}
trait AbstractValue extends Value {
  def abstractValue(ci: ConcreteInt): AbstractValue
}
case class SignInt(val negative: Boolean, val zero: Boolean, val positive: Boolean) extends AbstractValue {
  override def +(v: Value): SignInt = v match {
    case ci: ConcreteInt => SignInt.this + abstractValue(ci)
    case SignInt(nN, nZ, nP) => SignInt(nN || negative, (zero && nZ) || (positive && nN) || (negative && nP), nP || positive)
  }
  override def *(v: Value): SignInt = v match {
    case ci: ConcreteInt => this * abstractValue(ci)
    case SignInt(oN, oZ, oP) =>
      SignInt((oP && negative) || (oN && positive),
        oZ || zero,
        (oP && positive) || (oN && negative))
  }
  override def ==(v: Value): SignInt = v match {
    case ci: ConcreteInt => this == abstractValue(ci)
    case SignInt(oN, oZ, oP) =>
      val mayBeEqual = negative == oN || zero == oZ || positive == oP
      val mayBeUnequal = negative != oN || zero != oZ || positive != oP || negative || positive
      SignInt(false, mayBeUnequal, mayBeEqual)
  }
  def abstractValue(ci: ConcreteInt): SignInt = new SignInt(ci.v < 0, ci.v == 0, ci.v > 0)
  override def mayBeZero: Boolean = zero
  override def mayBeNonzero: Boolean = positive || negative
}
abstract class LessMoreInt extends AbstractValue {
  override def abstractValue(ci: ConcreteInt): LessMoreInt = ci.v match {
    case i: Int if i < -1 => Less
    case -1 => NegativeOne
    case 0 => Zero
    case 1 => One
    case i: Int if i > 1 => More
  }
  override def ==(v: Value) = v match {
    case ci: ConcreteInt => this == abstractValue(ci)
    // TODO imprecise (0 or 1)
    case Less => AnyLMI
    // TODO imprecise (0 or 1)
    case More => AnyLMI
    case AnyLMI => AnyLMI
    case lmi: LessMoreInt if lmi == this => One
    case lmi: LessMoreInt if lmi != this => Zero
  }
}
object Less extends LessMoreInt {
  override def +(v: Value): LessMoreInt = v match {
    case ci: ConcreteInt => this + abstractValue(ci)
    case More => AnyLMI
    // TODO imprecise in the case of One
    case lmi: LessMoreInt if lmi != More => Less
  }
  override def *(v: Value): LessMoreInt = v match {
    case ci: ConcreteInt => this * abstractValue(ci)
    case Less => More
    case NegativeOne => More
    case Zero => Zero
    case One => Less
    case More => Less
  }
  override def mayBeZero: Boolean = false
  override def mayBeNonzero: Boolean = true
}
object NegativeOne extends LessMoreInt {
  override def +(v: Value): LessMoreInt = v match {
    case ci: ConcreteInt => this + abstractValue(ci)
    case Less => Less
    case NegativeOne => Less
    case Zero => NegativeOne
    case One => Zero
    // TODO imprecise
    case More => AnyLMI
  }
  override def *(v: Value): LessMoreInt = v match {
    case ci: ConcreteInt => abstractValue(ci)
    case lmi: LessMoreInt => lmi match {
      case More => Less
      case One => NegativeOne
      case Zero => Zero
      case NegativeOne => One
      case Less => More
    }
  }
  override def mayBeZero: Boolean = false
  override def mayBeNonzero: Boolean = true
}
object Zero extends LessMoreInt {
  override def +(v: Value): LessMoreInt = v match {
    case ci: ConcreteInt => this + abstractValue(ci)
    case lmi: LessMoreInt => lmi
  }
  override def *(v: Value): LessMoreInt = this
  override def mayBeZero: Boolean = true
  override def mayBeNonzero: Boolean = false
}
object One extends LessMoreInt {
  override def +(v: Value): LessMoreInt = v match {
    case ci: ConcreteInt => this + abstractValue(ci)
    // TODO imprecise
    case Less => AnyLMI
    case NegativeOne => Zero
    case Zero => One
    case One => More
    case More => More
  }
  override def *(v: Value): LessMoreInt = v match {
    case ci: ConcreteInt => abstractValue(ci)
    case lmi: LessMoreInt => lmi
  }
  override def mayBeZero: Boolean = false
  override def mayBeNonzero: Boolean = true
}
object More extends LessMoreInt {
  override def +(v: Value): LessMoreInt = v match {
    case ci: ConcreteInt => this + abstractValue(ci)
    case Less => AnyLMI
    // TODO imprecise in the case of NegativeOne
    case lmi: LessMoreInt if lmi != Less => More
  }
  override def *(v: Value): LessMoreInt = v match {
    case ci: ConcreteInt => this * abstractValue(ci)
    case Less => Less
    case NegativeOne => Less
    case Zero => Zero
    case One => More
    case More => More
  }
  override def mayBeZero: Boolean = false
  override def mayBeNonzero: Boolean = true
}
object AnyLMI extends LessMoreInt {
  override def +(v: Value): LessMoreInt = this
  override def *(v: Value): LessMoreInt = this
  override def mayBeZero: Boolean = true
  override def mayBeNonzero: Boolean = true
}

sealed trait Storable

abstract sealed class Address
abstract sealed class ValueAddress extends Address
case class BindAddress(a: Int) extends ValueAddress
case class MonoAddress(v: Variable) extends ValueAddress
case class KontAddress(a: Int) extends Address
case object ResultAddress extends ValueAddress

case object StoreTypeException extends RuntimeException
case class Store[Stored <: Value: ClassTag](values: Map[ValueAddress, Stored], stack: Map[KontAddress, Kontinuation]) {
  def empty: Store[Stored] = Store(values.empty, stack.empty)
  def apply(va: ValueAddress): Stored = values(va)
  def apply(ka: KontAddress): Kontinuation = stack(ka)
  def +(a: ValueAddress, v: Stored): Store[Stored] = Store(values + Pair(a, v), stack)
  def +(ka: KontAddress, k: Kontinuation): Store[Stored] = Store(values, stack + Pair(ka, k))
  def +(p: Pair[Address, Storable]): Store[Stored] = p match {
    case (va: ValueAddress, s: Stored) => Store(values + Pair(va, s), stack)
    case (ka: KontAddress, k: Kontinuation) => Store(values, stack + Pair(ka, k))
    case _ => throw StoreTypeException
  }
  def ++(pairs: Pair[Address, Storable]*): Store[Stored] = pairs match {
    case Nil => this
    case pair :: rest => (this + pair) ++ rest
  }
  def ++(l: List[Pair[Address, Storable]]): Store[Stored] = l match {
    case Nil => this
    case pair :: rest => (this + pair) ++ rest
  }
  def -(va: ValueAddress): Store[Stored] = Store(values - va, stack)
  def -(ka: KontAddress): Store[Stored] = Store(values, stack - ka)
}

object TypeAliases {
  type Env = Map[Variable, Address]
  object Env {
    def apply(): Env = Map.empty
    def apply(v: Variable, a: Address): Env = Map(v -> a)
    def apply(pairs: Pair[Variable, Address]*): Env = Map(pairs: _*)
    def apply(l: List[Pair[Variable, Address]]): Env = Map(l: _*)
  }
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
  val none = new SignInt(false, false, false)
}
case class Label(l: String)
case class StackFrame(target: Int, previousEnv: Map[Variable, Address])

abstract sealed class Kontinuation extends Storable
case class ConcreteKontinuation[Stored <: Value: ClassTag](val env: Env, val taintedAddrs: Set[Address],
  val contextTaint: Set[Pair[Function, Int]], val f: Function, val ln: Int,
  val nextAddr: KontAddress) extends Kontinuation {
  def call(s: Store[Stored], result: Stored): State[Stored] =
    State(ln, f, env, s + (ResultAddress, result), taintedAddrs, contextTaint, s(nextAddr))
  def call(s: Store[Stored]): State[Stored] =
    State(ln, f, env, s, taintedAddrs, contextTaint, s(nextAddr))
}
object halt extends Kontinuation

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
