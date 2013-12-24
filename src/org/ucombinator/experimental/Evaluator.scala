package org.ucombinator.experimental

import TypeAliases._
import scala.reflect.ClassTag

case object Evaluator {
  def eval[Stored <: Value : ClassTag](exp: Expression, env: Env, store: Store[Stored]): Stored = {
    exp match {
      case s: Stored => s
      case Addition(lhs, rhs) =>
        val left = Converter[Stored](eval(lhs, env, store))
        val right = Converter[Stored](eval(rhs, env, store))
        Converter[Stored](left + right)
      case Multiplication(lhs, rhs) => Converter(eval(lhs, env, store) * eval(rhs, env, store))
      case Comparison(lhs, rhs) => Converter(eval(lhs, env, store) == eval(rhs, env, store))
    }
  }
}