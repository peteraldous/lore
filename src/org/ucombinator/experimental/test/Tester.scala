/*
    Implicit Flows: a prototype taint tracking system for implicit flows
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

package org.ucombinator.experimental.test

import scala.util.{ Try, Success, Failure }

abstract class Tester extends App {
  var passed = 0
  var run = 0
  def test(result: Boolean, tag: String): Unit = {
    run += 1
    if (result) {
      passed += 1
    } else {
      println("TEST FAILED: " + tag)
    }
  }

  def tryTest(r: Try[Any], tag: String) = {
    run += 1
    r match {
      case s: Success[Any] =>
        passed += 1
      case Failure(ex) =>
        println("TEST FAILED: " + tag + " " + ex.getClass() + " " + ex.getMessage)
    }
  }

  def tests: Unit

  tests
  println(passed + " of " + run + " tests passed")
}