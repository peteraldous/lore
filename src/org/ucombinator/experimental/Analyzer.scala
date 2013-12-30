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

import scala.io.Source
import TypeAliases._
import scala.reflect.ClassTag

object Analyzer extends App {
  def inject[Stored <: Value: ClassTag](functionTable: Map[String, Function]): State[Stored] = {
    State[Stored](0, functionTable("main"), Env(), Store(Map.empty, Map.empty), Set.empty, Set.empty, halt)
  }

  // TODO abstract garbage collection
  def explore[Stored <: Value: ClassTag](queue: List[State[Stored]], seen: Set[State[Stored]]): Set[State[Stored]] = queue match {
    case Nil => seen
    case state :: rest if seen contains state => explore(rest, seen)
    case state :: rest if !(seen contains state) => explore(rest ++ state.next, seen + state)
  }
  
  def significant[Stored <: Value](state: State[Stored]): Boolean = {
    state.f.isEndOfFunction(state.ln)
  }
  
  def print[Stored <: Value](state: State[Stored]): Unit = println(state)

  val functionTable = ToyParser.applyFuns(Source.fromInputStream(System.in).getLines.mkString)
  val allocator = MonovariantAllocator
  
  val allStates = explore(List(inject[SignInt](functionTable)), Set[State[SignInt]]())

  allStates filter significant map print
}