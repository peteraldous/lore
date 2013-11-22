package org.ucombinator.experimental

case class Function(val name: String, val params: List[Variable], val statements: List[Statement]) {
  case object StaticStatementException extends RuntimeException

  val labelTable = Map((statements filter { _ match { case l: LabelStatement => true; case _ => false } })
      map { _ match { case LabelStatement(label) => (label, statements.indexOf(label)) } }: _*)
  val catchDirectives = statements filter { _ match { case c: CatchDirective => true; case _ => false } }
  val handlers = catchDirectives map {
    _ match {
      case CatchDirective(begin, end, handler) =>
        Handler(labelTable(begin), labelTable(end), handler)
    }
  }
  
  def findExceptionHandler(line: Int): Option[Label] = {
    handlers filter { _ contains line } match {
      case Nil => None
      case head :: rest => Some(head.code)
    }
  }

  def successors(ln: Int): Set[Int] = {
    if (isEndOfFunction(ln)) Set.empty else statements(ln) match {
      case l: LabelStatement => Set(ln + 1)
      case a: AssignmentStatement => Set(ln + 1)
      case GotoStatement(l) => Set(labelTable(l))
      case IfStatement(e, l) => Set(labelTable(l), ln + 1)
      case _ => scala.sys.error("successors: unknown statement type")
    }
  }

  // TODO: memoize
  def mustReach(s: Int, seen: Set[Int] = Set.empty): Set[Int] = {
    if (seen contains s) {
      //      System.err.println("warning: loop. Termination leaks are possible.")
      Set.empty
    } else {
      if (isEndOfFunction(s)) Set.empty else {
        val nextSeen = seen + s
        statements(s) match {
          case as: AssignmentStatement => mustReach(s + 1, nextSeen) + (s + 1)
          case ls: LabelStatement => mustReach(s + 1, nextSeen) + (s + 1)
          case GotoStatement(l) => mustReach(labelTable(l), nextSeen) + labelTable(l)
          case IfStatement(cond, l) => mustReach(s + 1, nextSeen) & mustReach(labelTable(l), nextSeen)
        }
      }
    }
  }

  def isEndOfFunction(s: Int): Boolean = statements(s).isEndOfFunction
}