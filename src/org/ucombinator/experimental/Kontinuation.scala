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

import scala.reflect.ClassTag

import Env.Env

abstract sealed class Kontinuation extends Storable
case class ConcreteKontinuation[Stored <: Value: ClassTag](val env: Env, val taintedAddrs: Set[Address],
  val contextTaint: Set[Pair[Function, Int]], val loc: LineOfCode,
  val nextAddr: KontAddress) extends Kontinuation {
  // TODO I think contextTaint should be passed in, too
  def call(s: Store[Stored], ts: Set[Address]): Set[State[Stored]] =
    for (kont <- s(nextAddr)) yield State(loc, env, s, ts, contextTaint, kont)
}
object halt extends Kontinuation