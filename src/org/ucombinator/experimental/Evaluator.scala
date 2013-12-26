package org.ucombinator.experimental

import TypeAliases._
import scala.reflect.ClassTag

case object Evaluator {
  def eval[Stored <: Value : ClassTag](exp: Expression, env: Env, store: Store[Stored]): Stored = {
    exp match {
      case s: Stored => s
      case Addition(lhs, rhs) => Converter(eval(lhs, env, store) + eval(rhs, env, store))
      case Multiplication(lhs, rhs) => Converter(eval(lhs, env, store) * eval(rhs, env, store))
      case Comparison(lhs, rhs) => Converter(eval(lhs, env, store) == eval(rhs, env, store))
    }
  }
}