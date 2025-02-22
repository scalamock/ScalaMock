package org.scalamock.test.scalatest

import org.scalamock.stubs.Stubs
import org.scalamock.test.mockable.TestTrait
import org.scalatest.funspec.AnyFunSpec

class MySpec extends AnyFunSpec with Stubs {
  it("should work") {
    val m = stub[TestTrait]
  }
}
