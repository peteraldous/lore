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

case object NoSuchLabelException extends RuntimeException
case object StatementNumberOutOfBoundsException extends RuntimeException

case class LineOfCode(val ln: Int, f: Function) {
  def next: LineOfCode = LineOfCode(ln + 1, f)
  def jump(l: Label): LineOfCode = f.lookup(l)
  def statement: Statement = f.statements(ln)

  def isEndOfFunction: Boolean = if (f.statements isDefinedAt ln) {
    f.statements(ln).isEndOfFunction
  } else {
    // Errors if s refers to a statement not in the function.
    // This is reasonable because each function is capped with the FunctionEnd object.
    throw StatementNumberOutOfBoundsException
  }

  def findExceptionHandlerTarget: Option[LineOfCode] = {
    f.handlers filter { _ contains ln } match {
      case Nil => None
      case head :: rest => Some(f.lookup(head.code))
    }
  }

  def mustReach: Set[LineOfCode] = {
    val next = LineOfCode(ln + 1, f)
    if (f.statements isDefinedAt ln) {
      f.statements(ln) match {
        case l: LabelStatement => next.mustReach + next
        case a: AssignmentStatement => next.mustReach + next
        case GotoStatement(l) =>
          val target = f.lookup(l)
          target.mustReach + target
        case i: IfStatement => throw NotImplementedException
        case f: FunctionCall => throw NotImplementedException
        case r: ReturnStatement => Set(LineOfCode(f.statements.length, f))
        case t: ThrowStatement => throw NotImplementedException
        case c: CatchDirective => next.mustReach + next
        case f: FunctionDeclaration => throw NestedFunctionException
        case `FunctionEnd` => Set(next)
        case m: MoveResult => next.mustReach + next
      }
    } else Set.empty
  }
}