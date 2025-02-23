package org.scalamock.stubs.internal

import java.util.concurrent.atomic.AtomicReference

private[scalamock]
class StubUniqueIndexGenerator {
  private val uniqueIdx: AtomicReference[Int] = new AtomicReference(0)

  def nextIdx(): Int = uniqueIdx.updateAndGet(_ + 1)
}
