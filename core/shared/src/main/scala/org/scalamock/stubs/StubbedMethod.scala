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

trait StubbedMethod0[R] extends StubbedMethod.Order {
  type Result = R

  def returns(f: => Result): Unit

  def times: Int
}

trait StubbedMethod[A, R] extends StubbedMethod.Order {
  type Args = A
  type Result = R

  def returns(f: Args => Result): Unit

  def times: Int
  
  final def times(args: Args): Int = calls.count(_ == args)

  def calls: List[Args]
}

object StubbedMethod {
  sealed trait Order {
    def isBefore(other: Order)(implicit callLog: CallLog): Boolean

    def isAfter(other: Order)(implicit callLog: CallLog): Boolean
    
    def asString: String
  }
  
  class Internal[A, R](
    override val asString: String, 
    callLog: Option[CallLog],
    io: Option[StubIO]
  ) extends StubbedMethod[A, R]
      with StubbedMethod0[R] {
    override type Result = R
    override type Args = A

    override def toString = asString
    
    private val callsRef: AtomicReference[List[Args]] =
      new AtomicReference[List[Args]](Nil)

    private val resultRef: AtomicReference[Option[Args => Result]] =
      new AtomicReference[Option[Args => Result]](None)

    def impl(args: Args): Result =
      io match {
        case None =>
          callLog.foreach(_.internal.write(asString))
          callsRef.updateAndGet(args :: _)
          resultRef.get() match {
            case Some(f) => f(args)
            case None => throw new NotImplementedError()
          }
        case Some(io) =>
          io.flatMap(
            io.succeed {
              callLog.foreach(_.internal.write(asString))
              callsRef.updateAndGet(args :: _)
            }
          ) { _ =>
            resultRef.get() match {
              case Some(f) => f(args).asInstanceOf[io.F[Any, Any]]
              case None => io.die(new NotImplementedError())
            }
          }.asInstanceOf[Result]
      }

    def clear(): Unit = {
      callsRef.set(Nil)
      resultRef.set(None)
    }

    override def returns(f: Args => Result): Unit =
      resultRef.set(Some(f))

    override def returns(f: => Result): Unit =
      resultRef.set(Some(_ => f))

    override def times: Int =
      callsRef.get().length

    override def calls: List[Args] =
      callsRef.get().reverse

    override def isBefore(other: Order)(implicit callLog: CallLog): Boolean = {
      val actual = callLog.internal.calledMethods
      actual.indexOf(other.asString, actual.indexOf(asString)) != -1
    }

    override def isAfter(other: Order)(implicit callLog: CallLog): Boolean = {
      val actual = callLog.internal.calledMethods
      actual.indexOf(asString, actual.indexOf(other.asString)) != -1
    }
  }
}
