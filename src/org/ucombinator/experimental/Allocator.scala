package org.ucombinator.experimental

abstract sealed class Allocator {
  def alloc(v: Variable): ValueAddress
}

// TODO make more allocators

case object MonovariantAllocator extends Allocator {
  override def alloc(v: Variable): ValueAddress = MonoAddress(v)
}