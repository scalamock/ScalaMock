package org.scalamock.stubs

import org.scalamock.stubs.internal.NotGiven

import scala.annotation.experimental
import scala.quoted.{Expr, Quotes, Type}

private
inline def stubImpl[T](using createdStubs: internal.CreatedStubs): Stub[T] =
  ${ stubMacro[T]('{ createdStubs }) }

private
inline def stubbed0Impl[R](inline f: => R) = ${ stubbed0Macro[R]('{ f }) }

private
inline def stubbed1Impl[T1, R](inline f: T1 => R)(using NotGiven[T1 <:< Tuple]) = ${ stubbed1Macro[T1, R]('{ f }) }

private
inline def stubbedImpl[Args <: NonEmptyTuple, R](inline f: Args => R) =
  ${ stubbedMacro[Args, R]('{ f }) }


@experimental
private
def stubMacro[T: Type](collector: Expr[internal.CreatedStubs])(using Quotes): Expr[Stub[T]] =
  new internal.StubMaker().newInstance[T](collector)

@experimental
private
def stubbed0Macro[R: Type](f: Expr[R])(using Quotes): Expr[StubbedMethod[Unit, R]] =
  new internal.StubMaker().getStubbed0[R](f)

@experimental
private
def stubbed1Macro[T1: Type, R: Type](f: Expr[T1 => R])(using Quotes): Expr[StubbedMethod[T1, R]] =
  new internal.StubMaker().getStubbed1[T1, R](f)

@experimental
private
def stubbedMacro[Args <: NonEmptyTuple: Type, R: Type](f: Expr[Args => R])(using Quotes): Expr[StubbedMethod[Args, R]] =
  new internal.StubMaker().getStubbed[Args, R](f)



