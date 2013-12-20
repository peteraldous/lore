package org.ucombinator.experimental

import TypeAliases._

case object Evaluator {
  def eval[Stored <: Value](exp: Expression, env: Env, store: Store[Stored]): Stored = exp match {
    case s: Stored => s
  }
}