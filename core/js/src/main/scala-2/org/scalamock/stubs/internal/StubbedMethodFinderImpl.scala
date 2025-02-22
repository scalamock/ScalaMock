package org.scalamock.stubs.internal

import org.scalamock.stubs.StubbedMethod
import org.scalamock.util.MacroAdapter

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

    val fullName = TermName(mockFunctionName(name, obj.tpe, targs))


    val code = c.Expr[StubbedMethod[Args, R]](
      q"""{
      import scala.scalajs.js
      $obj.asInstanceOf[js.Dynamic].$fullName.asInstanceOf[${weakTypeOf[StubbedMethod[Args, R]]}]
    }""")
    code
  }
}
