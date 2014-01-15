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

trait Storable

case object StoreTypeException extends RuntimeException
case class Store[Stored <: Value: ClassTag](values: Map[ValueAddress, Stored],
  stack: Map[KontAddress, Set[Kontinuation]]) {
  def apply(va: ValueAddress): Stored = values(va)
  def apply(ka: KontAddress): Set[Kontinuation] = stack(ka)
  def +(a: ValueAddress, v: Stored): Store[Stored] = Store(values + Pair(a, v), stack)
  def +(ka: KontAddress, k: Kontinuation): Store[Stored] = if (stack isDefinedAt ka) {
    Store(values, stack + Pair(ka, stack(ka) + k))
  } else {
    Store(values, stack + Pair(ka, Set(k)))
  }
  def +(p: Pair[Address, Storable]): Store[Stored] = p match {
    case (va: ValueAddress, s: Stored) => Store(values + Pair(va, s), stack)
    case (ka: KontAddress, k: Kontinuation) => this.+(ka, k)
    case _ => throw StoreTypeException
  }
  def ++(pairs: Pair[Address, Storable]*): Store[Stored] = pairs match {
    case Nil => this
    case pair :: rest => (this + pair) ++ rest
  }
  def ++(l: List[Pair[Address, Storable]]): Store[Stored] = l match {
    case Nil => this
    case pair :: rest => (this + pair) ++ rest
  }
  def -(va: ValueAddress): Store[Stored] = Store(values - va, stack)
  def -(ka: KontAddress): Store[Stored] = Store(values, stack - ka)
}