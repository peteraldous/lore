package org.ucombinator.experimental

import scala.reflect.ClassTag

case object UnconvertibleException extends RuntimeException

case object Converter {
  def apply[Stored <: Value : ClassTag](v: Value): Stored = v match {
    case s: Stored => s
    case _ => throw UnconvertibleException
  }
}
