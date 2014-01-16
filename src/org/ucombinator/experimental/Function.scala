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

case class Function(val name: String, val params: List[Variable], val statements: List[Statement]) {
  case object StaticStatementException extends RuntimeException

  val labelStatements = statements flatMap {
    _ match {
      case l @ LabelStatement(label) => Some(Pair(label, statements.indexOf(l)))
      case _ => None
    }
  }
  val labelTable = Map(labelStatements: _*)
  val handlers = statements flatMap {
    _ match {
      case CatchDirective(begin, end, handler) =>
        Some(ExceptionHandler(labelTable(begin), labelTable(end), handler))
      case _ => None
    }
  }

  def lookup(l: Label): LineOfCode = {
    val target = labelTable.get(l) match {
      case Some(i) => i
      case None => throw NoSuchLabelException
    }
    LineOfCode(target, this)
  }
  
  def init: LineOfCode = LineOfCode(0, this)
}