package org.ucombinator.experimental

import TypeAliases._

case class State[Stored <: Value](val ln: Int, val f: Function, val env: Env, val store: Store[Stored], val taintStore: Set[Address], val contextTaint: Set[Int], val stack: Kontinuation) {

}