// Copyright (c) 2011-2025 ScalaMock Contributors (https://github.com/ScalaMock/ScalaMock/graphs/contributors)
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

package org.scalamock.stubs

import org.scalamock.stubs.internal.NotGiven

import scala.language.implicitConversions

/** Indicates that object of type T was generated */
opaque type Stub[+T] <: T = T

trait Stubs {
  /** Collects all generated stubs */
  final given stubs: internal.CreatedStubs = internal.CreatedStubs()

  /**
   *  Resets all recorded stub functions and arguments.
   *  This is useful if you want to create your stub once per suite.
   *  Note that in such case test cases should run sequentially
   */
  final def resetStubs(): Unit = stubs.clearAll()

  /** Generates an object of type T with possibility to record methods arguments and set-up method results */
  inline def stub[T]: Stub[T] = stubImpl[T]

  implicit inline def stubbed[R](inline f: => R): StubbedMethod[Unit, R] =
    stubbed0Impl[R](f)

  implicit inline def stubbed[T1, R](inline f: T1 => R)(using notGiven: NotGiven[T1 <:< Tuple]): StubbedMethod[T1, R] =
    stubbed1Impl[T1, R](f)

  implicit inline def stubbed[Args <: ? *: NonEmptyTuple, R](inline f: Args => R): StubbedMethod[Args, R] =
    stubbedImpl[Args, R](f)
}
