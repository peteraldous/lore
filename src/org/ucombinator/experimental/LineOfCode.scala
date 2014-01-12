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

case class LineOfCode(val ln: Int, val f: Function) {
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

  // TODO this probably can loop infinitely
  // TODO check this function for correctness
  // TODO handle the possibility of a top-level exception if f is "main"
  def possibleCatchers: Set[LineOfCode] = {
    findExceptionHandlerTarget match {
      case Some(t) => Set(t)
      case None =>
        val sets = for {
        callSite <- Analyzer.callSites(f)
      } yield callSite.possibleCatchers
      sets flatMap {s => s}
    }
  }

  def mustReach: Set[LineOfCode] = {
    statement match {
      case l: LabelStatement => next.mustReach + next
      case a: AssignmentStatement => next.mustReach + next
      case GotoStatement(l) =>
        val target = jump(l)
        target.mustReach + target
      case IfStatement(c, l) =>
        val target = jump(l)
        target.mustReach & next.mustReach
      case FunctionCall(f, exps) =>
        val target = Analyzer.functionTable(f).init
        target.mustReach + target
      // TODO is it sound to use the kontinuation?
      case r: ReturnStatement => throw NotImplementedException
      // TODO make a static collection of all exception handler targets whose
      // handlers could possibly handle an exception from this LoC
      case t: ThrowStatement => throw NotImplementedException
      case c: CatchDirective => next.mustReach + next
      case f: FunctionDeclaration => throw NestedFunctionException
      // TODO same as ReturnStatement(void)
      case `FunctionEnd` => throw NotImplementedException
      case m: MoveResult => next.mustReach + next
    }
  }
}