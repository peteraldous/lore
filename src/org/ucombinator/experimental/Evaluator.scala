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

case object KontinuationEvaluationException extends RuntimeException

case object Evaluator {
  def eval[Stored <: Value: ClassTag](exp: Expression, env: Env, store: Store[Stored]): Stored = {
    exp match {
      case s: Stored => s
      // TODO this line is heinous. Make it better.
      case ci: ConcreteInt => Converter(Converter(store(BunkAddress)).abstractValue(ci))
      case Addition(lhs, rhs) => Converter(eval(lhs, env, store) + eval(rhs, env, store))
      case Multiplication(lhs, rhs) => Converter(eval(lhs, env, store) * eval(rhs, env, store))
      case Comparison(lhs, rhs) => Converter(eval(lhs, env, store) == eval(rhs, env, store))
      case v: Variable =>
        val address = env(v)
        address match {
          case va: ValueAddress => store(va)
          case ka: KontAddress => throw KontinuationEvaluationException
        }
    }
  }

  def tainted(exp: Expression, env: Env, taintStore: Set[Address]): Boolean = {
    def innerTainted(exp: Expression): Boolean = {
      exp match {
        case Addition(lhs, rhs) => innerTainted(lhs) || innerTainted(rhs)
        case Multiplication(lhs, rhs) => innerTainted(lhs) || innerTainted(rhs)
        case Comparison(lhs, rhs) => innerTainted(lhs) || innerTainted(rhs)
        case v: Variable => taintStore contains env(v)
        case v: Value => false
      }
    }
    innerTainted(exp)
  }
}