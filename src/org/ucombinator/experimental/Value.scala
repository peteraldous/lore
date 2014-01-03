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
case class LessMoreInt(val less: Boolean, val negativeOne: Boolean, val zero: Boolean,
  val one: Boolean, val more: Boolean) extends AbstractValue {
  override def abstractValue(ci: ConcreteInt): LessMoreInt = {
    val v = ci.v
    LessMoreInt(v < -1, v == -1, v == 0, v == 1, v > 1)
  }
  override def ==(v: Value) = v match {
    case ci: ConcreteInt => this == abstractValue(ci)
    case lmi: LessMoreInt => if (this equals lmi) {
      LessMoreInt(false, false, false, true, false)
    } else {
      LessMoreInt(false, false, true, false, false)
    }
  }
  override def +(v: Value) = v match {
    case ci: ConcreteInt => this + abstractValue(ci)
    case lmi: LessMoreInt =>
      val couldBeLess = lmi.less || less || (lmi.negativeOne && negativeOne)
      val couldBeNegativeOne = (
        (lmi.zero && negativeOne) ||
        (lmi.negativeOne && zero) ||
        (lmi.less && one) ||
        (lmi.one && less) ||
        (lmi.less && more) ||
        (lmi.more && less))
      val couldBeZero = (
        (lmi.zero && zero) ||
        (lmi.negativeOne && one) ||
        (lmi.one && negativeOne) ||
        (lmi.less && more) ||
        (lmi.more && less))
      val couldBeOne = (
        (lmi.zero && one) ||
        (lmi.one && zero) ||
        (lmi.more && negativeOne) ||
        (lmi.negativeOne && more) ||
        (lmi.less && more) ||
        (lmi.more && less))
      val couldBeMore = lmi.more || more || (lmi.one && one)
      LessMoreInt(couldBeLess, couldBeNegativeOne, couldBeZero, couldBeOne, couldBeMore)
  }
  override def *(v: Value) = v match {
    case ci: ConcreteInt => this + abstractValue(ci)
    case lmi: LessMoreInt =>
      val couldBeLess = (
        (lmi.less && (one || more)) ||
        ((lmi.one || lmi.more) && less) ||
        (lmi.more && negativeOne) ||
        (lmi.negativeOne && more))
      val couldBeNegativeOne = (lmi.one && negativeOne) || (lmi.negativeOne && one)
      val couldBeZero = lmi.zero || zero
      val couldBeOne = (lmi.one && one) || (lmi.negativeOne && negativeOne)
      val couldBeMore = (
        (lmi.more && (one || more)) ||
        ((lmi.negativeOne || lmi.less) && less) ||
        (lmi.less && negativeOne) ||
        (lmi.one && more))
      LessMoreInt(couldBeLess, couldBeNegativeOne, couldBeZero, couldBeOne, couldBeMore)
  }
  override def mayBeZero: Boolean = zero
  override def mayBeNonzero: Boolean = less || negativeOne || one || more
}