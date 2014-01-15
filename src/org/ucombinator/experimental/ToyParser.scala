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
import scala.util.parsing.combinator.RegexParsers

case class ParseError(msg: String) extends RuntimeException

object ToyParser extends RegexParsers {

  def labelStatement: Parser[Statement] = "(label" ~> label <~ ")" ^^ { LabelStatement(_) }
  def gotoStatement: Parser[Statement] = "(goto" ~> label <~ ")" ^^ { GotoStatement(_) }
  def assignStatement: Parser[Statement] = "(:=" ~> variable ~ expr <~ ")" map {
    case variable ~ expression => AssignmentStatement(variable, expression)
    case _ => scala.sys.error("could not match assignment statement")
  }
  def condStatement: Parser[Statement] = "(if" ~> expr ~ label <~ ")" map {
    case expression ~ label => IfStatement(expression, label)
    case _ => scala.sys.error("could not match if statement")
  }
  def functionCall: Parser[Statement] = "(invoke" ~> variable ~ rep(expr) <~ ")" map {
    case variable ~ expressions => FunctionCall(variable.v, expressions)
    case _ => throw new ParseError("Bad function call")
  }
  def returnStatement: Parser[Statement] = "(return" ~> expr <~ ")" ^^ { ReturnStatement(_) }
  def throwStatement: Parser[Statement] = "(throw " ~> expr <~ ")" ^^ { ThrowStatement(_) }
  def catchDirective: Parser[Statement] = "(catch " ~> label ~ label ~ label <~ ")" map {
    case begin ~ end ~ handler => CatchDirective(begin, end, handler)
    case _ => throw new ParseError("Bad catch directive")
  }
  def functionDeclaration: Parser[Statement] = "(fun " ~> variable ~ rep(variable) <~ ")" map {
    case name ~ variables => FunctionDeclaration(name.v, List(variables: _*))
    case _ => throw new ParseError("Bad function declaration")
  }
  def functionEnd: Parser[Statement] = "(endfun)" ^^ { _ => FunctionEnd }
  def moveResult: Parser[Statement] = "(result " ~> variable <~ ")" ^^ { MoveResult(_) }

  def label: Parser[Label] = "_[a-z]+".r ^^ { Label(_) }
  def variable: Parser[Variable] = "[a-z]+".r ^^ { Variable(_) }
  def expr: Parser[Expression] = addition | multiplication | comparison | variable | value

  def value: Parser[Value] = "[0-9]+".r ^^ { (integer) => ConcreteInt(integer.toInt) }
  def addition: Parser[Addition] = "(+" ~> expr ~ expr <~ ")" ^^ { (result) => Addition(result._1, result._2) }
  def multiplication: Parser[Multiplication] = "(*" ~> expr ~ expr <~ ")" ^^ { (result) => Multiplication(result._1, result._2) }
  def comparison: Parser[Comparison] = "(=" ~> expr ~ expr <~ ")" ^^ { (result) => Comparison(result._1, result._2) }

  def stmt: Parser[Statement] = (labelStatement | gotoStatement | assignStatement | condStatement
    | functionCall | returnStatement | catchDirective | moveResult | throwStatement)

  def fun: Parser[Function] = functionDeclaration ~ rep(stmt) ~ functionEnd map {
    case decl ~ stmts ~ end => decl match {
      case FunctionDeclaration(name, vars) => Function(name, vars, stmts :+ end)
      case _ => throw new ParseError("Could not deconstruct FunctionDeclaration")
    }
    case _ => throw new ParseError("Bad function")
  }

  case class DuplicateFunctionNameException(fn: String) extends RuntimeException
  def applyFuns(input: String): Map[String, Function] = {
    def processParseResult(result: ParseResult[Function]): Map[String, Function] = result match {
      case Success(result, remainder) =>
        val soFar = innerApplyFuns(remainder)
        if (soFar isDefinedAt result.name)
          throw DuplicateFunctionNameException(result.name)
        else
          soFar + Pair(result.name, result)
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
    def innerApplyFuns(input: Input): Map[String, Function] = {
      if (input.atEnd) Map.empty else processParseResult(parse(fun, input))
    }
    processParseResult(parse(fun, input))
  }

  def applyStmt(input: String): Statement = parseAll(stmt, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

  def applyStmts(input: String): List[Statement] = {
    def processParseResult(result: ParseResult[Statement]): List[Statement] = result match {
      case Success(result, remainder) => result :: innerApplyStmts(remainder)
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }
    def innerApplyStmts(input: Input): List[Statement] = {
      if (input.atEnd) List.empty else processParseResult(parse(stmt, input))
    }
    processParseResult(parse(stmt, input))
  }

  def applyExpr(input: String): Expression = parseAll(expr, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

  def applyLabel(input: String): Label = parseAll(label, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}