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

package org.scalamock.stubs.internal

import org.scalamock.util.MacroAdapter
import org.scalamock.stubs.StubbedMethod

private[scalamock]
object StubbedMethodFinderImpl {
  import MacroAdapter.Context

  def find[
    Args: c.WeakTypeTag,
    R: c.WeakTypeTag
  ](
    c: Context
  )(
    obj: c.Tree,
    name: c.Name,
    targs: List[c.Type],
    actuals: List[c.universe.Type]
  ): c.Expr[StubbedMethod[Args, R]] = {
    import c.universe._

    def mockFunctionName(name: Name, t: Type, targs: List[Type]) = {
      val method = t.member(name).asTerm
      if (method.isOverloaded)
        "stub$" + name + "$" + method.alternatives.indexOf(StubbedMethodFinder.resolveOverloaded(c)(method, targs, actuals))
      else
        "stub$" + name + "$0"
    }

    val code = c.Expr[StubbedMethod[Args, R]](q"""
      $obj
        .getClass()
        .getMethod(${mockFunctionName(name, obj.tpe, targs)})
        .invoke($obj)
        .asInstanceOf[${weakTypeOf[StubbedMethod[Args, R]]}]
     """
    )
    code
  }
}
