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

import org.ucombinator.experimental.Addition
import org.ucombinator.experimental.AssignmentStatement
import org.ucombinator.experimental.Comparison
import org.ucombinator.experimental.FunctionCall
import org.ucombinator.experimental.GotoStatement
import org.ucombinator.experimental.IfStatement
import org.ucombinator.experimental.Label
import org.ucombinator.experimental.LabelStatement
import org.ucombinator.experimental.Multiplication
import org.ucombinator.experimental.ReturnStatement
import org.ucombinator.experimental.ToyParser
import org.ucombinator.experimental.ConcreteInt
import org.ucombinator.experimental.Variable
import org.ucombinator.experimental.CatchDirective
import org.ucombinator.experimental.MoveResult
import scala.util.{Try, Success, Failure}
import org.ucombinator.experimental.FunctionEnd
import org.ucombinator.experimental.ThrowStatement

object ParserTester extends Tester {

  override def tests: Unit = {
    test(ToyParser.applyExpr("2") == ConcreteInt(2), "basic value")
    test(ToyParser.applyExpr("29") == ConcreteInt(29), "two-digit value")
    test(ToyParser.applyLabel("_l") == Label("_l"), "basic label")
    test(ToyParser.applyExpr("v") == Variable("v"), "basic variable")
    test(ToyParser.applyExpr("(+ 1 2)") == Addition(ConcreteInt(1), ConcreteInt(2)), "basic addition")
    test(ToyParser.applyExpr("(* 1 2)") == Multiplication(ConcreteInt(1), ConcreteInt(2)), "basic multiplication")
    test(ToyParser.applyExpr("(= 1 2)") == Comparison(ConcreteInt(1), ConcreteInt(2)), "basic comparison")
    test(ToyParser.applyExpr("(= (+ 2 3) (* 8 7))") == Comparison(Addition(ConcreteInt(2), ConcreteInt(3)), Multiplication(ConcreteInt(8), ConcreteInt(7))), "nested arithmetic")
    test(ToyParser.applyExpr("(+ 1 v)") == Addition(ConcreteInt(1), Variable("v")), "variable in addition")
    test(ToyParser.applyStmt("(label _l)") == LabelStatement(Label("_l")), "basic label")
    test(ToyParser.applyStmt("(goto _l)") == GotoStatement(Label("_l")), "basic goto")
    test(ToyParser.applyStmt("(:= v 1)") == AssignmentStatement(Variable("v"), ConcreteInt(1)), "basic assignment")
    test(ToyParser.applyStmt("(if 1 _l)") == IfStatement(ConcreteInt(1), Label("_l")), "basic conditional")
    test(ToyParser.applyStmt("(:= y (+ 1 2))") == AssignmentStatement(Variable("y"), Addition(ConcreteInt(1), ConcreteInt(2))), "nested addition in assignment")
    test(ToyParser.applyStmts("(label _l)(goto _l)") == List(LabelStatement(Label("_l")), GotoStatement(Label("_l"))), "two statements")
    test(ToyParser.applyStmt("(invoke noargs)") == FunctionCall("noargs", List.empty), "thunk application")
    test(ToyParser.applyStmt("(invoke onearg 2)") == FunctionCall("onearg", List(ConcreteInt(2))), "one-argument application")
    test(ToyParser.applyStmt("(invoke twoargs 2 7)") == FunctionCall("twoargs", List(ConcreteInt(2), ConcreteInt(7))), "two-argument application")
    test(ToyParser.applyStmt("(return 1)") == ReturnStatement(ConcreteInt(1)), "return statement")
    test(ToyParser.applyStmt("(catch _b _e _h)") == CatchDirective(Label("_b"), Label("_e"), Label("_h")), "catch directive")
    test(ToyParser.applyStmt("(result x)") == MoveResult(Variable("x")), "function result")
    test(Try(ToyParser.parse(ToyParser.functionDeclaration, "(fun zero)")).isSuccess, "function declaration with no params")
    test(Try(ToyParser.parse(ToyParser.functionDeclaration, "(fun one arg)")).isSuccess, "function declaration with one parameter")
    test(Try(ToyParser.parse(ToyParser.functionDeclaration, "(fun two x y)")).isSuccess, "function declaration with two params")
    test(ToyParser.parse(ToyParser.functionEnd, "(endfun)").get == FunctionEnd, "function end")
    tryTest(Try(ToyParser.parse(ToyParser.fun,"(fun main)(endfun)")), "parse single function with no statements")
    tryTest(Try(ToyParser.parse(ToyParser.fun,"(fun zero)(return 0)(endfun)")), "parse single function with one statement")
    test(Try(ToyParser.parse(ToyParser.fun,"(fun main)(return 0)(endfun)")).isSuccess, "parse single function with one statement")
    test(Try(ToyParser.applyFuns("(fun zero)(endfun)(fun main)(endfun)")).isSuccess, "parse multiple functions without error")
    tryTest(Try(ToyParser.applyFuns("(fun double var)(return (* var 2))(endfun) (fun main)(:= y (+ 1 x))(invoke double y)(result z)(endfun)")), "parse two functions with statements without error")
    test(ToyParser.applyStmt("(throw 1)") == ThrowStatement(ConcreteInt(1)), "throw statement")
    test(ToyParser.applyStmt("(catch _b _e _h)") == CatchDirective(Label("_b"), Label("_e"), Label("_h")), "catch directive")
  }
}