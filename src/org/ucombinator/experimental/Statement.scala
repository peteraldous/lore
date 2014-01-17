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

case class Label(l: String)

abstract sealed class Statement {
  def isEndOfFunction: Boolean
  override def toString: String = ""
}
case class LabelStatement(val l: Label) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(Label " + l + ")\""
}
case class AssignmentStatement(val v: Variable, val e: Expression) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(:= " + v + " " + e + ")\""
}
case class GotoStatement(val l: Label) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(Goto " + l + ")\""
}
case class IfStatement(val condition: Expression, val l: Label) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(If " + condition + " " + l + ")\""
}
case class FunctionCall(val fun: String, val exps: List[Expression]) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(invoke " + fun + " " + (exps mkString " ") + ")\""
}
case class ReturnStatement(val e: Expression) extends Statement {
  override def isEndOfFunction: Boolean = true
  override def toString: String = super.toString + "\"(return " + e + ")\""
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
  override def toString: String = super.toString + "\"(fun " + name + " " + (vars mkString " ") + ")\""
}
case object FunctionEnd extends Statement {
  override def isEndOfFunction: Boolean = true
  override def toString: String = super.toString + "\"(endfun)\""
}
case class MoveResult(val v: Variable) extends Statement {
  override def isEndOfFunction: Boolean = false
  override def toString: String = super.toString + "\"(result " + v + ")\""
}