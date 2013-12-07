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

case class Function(val name: String, val params: List[Variable], val statements: List[Statement]) {
  case object StaticStatementException extends RuntimeException

  val labelStatements = statements filter { _ match { case l: LabelStatement => true; case _ => false } }
  val labelTable = Map(labelStatements
    map {
      _ match {
        case LabelStatement(label) => (label, statements.indexOf(label))
        case _ => throw ImpossibleException
      }
    }: _*)
  val handlers = (statements filter { _ match { case c: CatchDirective => true; case _ => false } }) map {
    _ match {
      case CatchDirective(begin, end, handler) =>
        ExceptionHandler(labelTable(begin), labelTable(end), handler)
      case _ => throw ImpossibleException
    }
  }

  def findExceptionHandlerTarget(line: Int): Option[Label] = {
    handlers filter { _ contains line } match {
      case Nil => None
      case head :: rest => Some(head.code)
    }
  }

  def isEndOfFunction(s: Int): Boolean = statements(s).isEndOfFunction
}