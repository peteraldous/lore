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

import scala.language.postfixOps
import scala.collection.immutable.HashMap
import TypeAliases._
import scala.collection.GenTraversableOnce
import scala.reflect.ClassTag

case object NotImplementedException extends RuntimeException
case object ImpossibleException extends RuntimeException

trait Storable

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
}
case class Label(l: String)

// states are (ln, env, store)
// configurations are (state, stack summary)
