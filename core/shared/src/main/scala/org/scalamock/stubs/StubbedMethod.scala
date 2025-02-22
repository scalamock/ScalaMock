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

import java.util.concurrent.atomic.AtomicReference

trait StubbedMethod[Args, R] {
  /**
   * Allows to set result for method with arguments.
   * {{{
   *   trait Foo {
   *     def foo1(x: Int): Int
   *     def foo2(x: Int, y: Int): Int
   *   }
   *
   *   val foo = stub[Foo]
   *
   *   foo.foo1.returns {
   *     case 1 => 2
   *     case 2 => 5
   *     case _ => 0
   *   }
   *
   *   foo.foo2.returns {
   *     case (0, 0) => 1
   *     case _ => 0
   *   }
   *
   *   foo.foo1(2) // 5
   *   foo.foo2(0, 0) // 1
   *   }}}
   */
  def returns(f: Args => R)(implicit notUnit: internal.NotGiven[Args =:= Unit]): Unit

  /** Allows to set result for method without arguments.
   * {{{
   *   trait Foo {
   *     def foo0: Int
   *.  }
   *
   *   val foo = stub[Foo]
   *
   *   foo.foo0.returns(5)
   *   foo.foo0 // 5
   *   }}}
   * */
  def returns(f: R)(implicit unit: Args =:= Unit): Unit


  /**
   * Allows to get number of times method was executed.
   * {{{
   *   trait Foo {
   *     def foo1(x: Int): Int
   *.  }
   *   val foo = stub[Foo]
   *   foo.foo1.returns(_ => 1)
   *
   *   foo.foo1(2)
   *   foo.foo1(3)
   *
   *   foo.foo1.times // 2
   * }}}
   */
  def times: Int

  /**
   * Allows to get caught method arguments.
   * For multiple arguments - returns them tupled.
   * One list item per call.
   * {{{
   *   trait Foo {
   *     def foo1(x: Int): Int
   *     def foo2(x: Int, y: Int): Int
   *   }
   *
   *   val foo = stub[Foo]
   *
   *   foo.foo1.returns(_ => 1)
   *   foo.foo2.returns(_ => 1)
   *
   *   foo.foo1(2)
   *   foo.foo1(2)
   *   foo.foo2(0, 1)
   *   foo.foo2(2, 3)
   *
   *   foo.foo1.calls // List(2, 2)
   *   foo.foo2.calls // List((0, 1), (2, 3))
   * }}}
   */
  def calls(implicit notUnit: internal.NotGiven[Args =:= Unit]): List[Args]
}

object StubbedMethod {
  class Internal[Args, R] extends StubbedMethod[Args, R] {
    private[scalamock] val callsRef: AtomicReference[List[Args]] =
      new AtomicReference[List[Args]](Nil)

    private[scalamock] val resultRef: AtomicReference[Option[Args => R]] =
      new AtomicReference[Option[Args => R]](None)

    def impl(args: Args) = {
      callsRef.updateAndGet(args :: _)
      resultRef.get() match {
        case Some(f) => f(args)
        case None => throw new NotImplementedError()
      }
    }

    def clear(): Unit = {
      callsRef.set(Nil)
      resultRef.set(None)
    }

    def returns(f: Args => R)(implicit not: internal.NotGiven[Args =:= Unit]): Unit =
      resultRef.set(Some(f))

    def returns(f: R)(implicit ev: Args =:= Unit): Unit =
      resultRef.set(Some(_ => f))

    def times: Int =
      callsRef.get().length

    def calls(implicit not: internal.NotGiven[Args =:= Unit]): List[Args] =
      callsRef.get().reverse
  }
}
