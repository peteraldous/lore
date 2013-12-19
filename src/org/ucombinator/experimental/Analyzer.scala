package org.ucombinator.experimental

import scala.io.Source

object Analyzer extends App {
  val functionTable = ToyParser.applyFuns(Source.fromInputStream(System.in).getLines.mkString)
  val allocator = MonovariantAllocator
}