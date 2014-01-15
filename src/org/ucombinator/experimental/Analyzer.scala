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
import scala.reflect.ClassTag
import scala.language.postfixOps

object Analyzer extends App {
  def inject[Stored <: Value: ClassTag](functionTable: Map[String, Function]): State[Stored] = {
    RegularState[Stored](functionTable("main").init, Env(), Store(Map.empty, Map.empty), Set.empty, Set.empty, halt)
  }

  // TODO abstract garbage collection - remember to look at all of the environments in the stack
  def explore[Stored <: Value: ClassTag](queue: List[State[Stored]], seen: Set[State[Stored]]): Set[State[Stored]] = queue match {
    case Nil => seen
    case state :: rest if seen contains state => explore(rest, seen)
    case state :: rest if !(seen contains state) => explore(rest ++ state.next, seen + state)
  }

  def significant[Stored <: Value](state: State[Stored]): Boolean = {
    state match {
      case RegularState(loc, env, store, ts, ct, k) => loc.statement match {
        case FunctionCall(name, exps) if name == "leak" =>
          def anExpIsTainted(exps: List[Expression]): Boolean = {
            exps match {
              case Nil => false
              case exp :: rest if Evaluator.tainted(exp, env, ts) => true
              case exp :: rest if !Evaluator.tainted(exp, env, ts) => anExpIsTainted(rest)
            }
          }
          !(ct isEmpty) || anExpIsTainted(exps)
        case _ => false
      }
      case _ => false
    }
  }

  case class CallerMap(m: Map[Function, Set[LineOfCode]]) {
    def +(p: Pair[Function, LineOfCode]): CallerMap = this.+(p._1, p._2)
    def +(f: Function, loc: LineOfCode): CallerMap = {
      if (m isDefinedAt f)
        CallerMap(m + Pair(f, m(f) + loc))
      else
        CallerMap(m + Pair(f, Set(loc)))
    }
    def ++(f: Function): CallerMap = this ++ f.init
    def ++(loc: LineOfCode): CallerMap = loc.statement match {
      case `FunctionEnd` => this
      case FunctionCall(fn, exps) => (this.+(functionTable(fn), loc)) ++ loc.next
      case _ => this ++ loc.next
    }
    def ++(fs: Iterable[Function]): CallerMap = fs match {
      case Nil => this
      case head :: rest => (this ++ head) ++ rest
    }
    def apply(f: Function): Set[LineOfCode] = m(f)
  }
  case object CallerMap {
    def empty: CallerMap = new CallerMap(Map.empty)
    def apply(f: Function): CallerMap = empty ++ f
    def apply(fs: Iterable[Function]): CallerMap = empty ++ fs
  }

  case class CatcherMap(m: Map[LineOfCode, Set[LineOfCode]]) {
    def addPair(p: Pair[LineOfCode, LineOfCode]): CatcherMap = this.addPair(p._1, p._2)
    def addPair(tloc: LineOfCode, cloc: LineOfCode): CatcherMap = {
      if (m isDefinedAt tloc)
        CatcherMap(m + Pair(tloc, m(tloc) + cloc))
      else
        CatcherMap(m + Pair(tloc, Set(cloc)))
    }
    def +(tloc: LineOfCode): CatcherMap = {
      throw NotImplementedException
    }
  }

  def print[Stored <: Value](state: State[Stored]): Unit = println(state)

  val functionTable = ToyParser.applyFuns(Source.fromInputStream(System.in).getLines.mkString)
  val callSites = CallerMap(functionTable.values)
  val allocator = MonovariantAllocator
  val kontAllocator = CallSiteAllocator

  val allStates = explore(List(inject[SignInt](functionTable)), Set[State[SignInt]]())

  allStates filter significant map print
}