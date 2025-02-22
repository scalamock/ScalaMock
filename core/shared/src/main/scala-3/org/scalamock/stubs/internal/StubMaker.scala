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

import org.scalamock.clazz.Utils
import org.scalamock.stubs.{CallLog, Stub, StubIO, StubbedMethod}

import scala.annotation.{experimental, tailrec}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.quoted.{Expr, Quotes, Type}

private[stubs] class StubMaker(
  using Quotes
) extends Utils:
  import quotes.reflect.*

  override def newApi = true

  @experimental
  def newInstance[T: Type](
    collector: Expr[CreatedStubs]
  ): Expr[Stub[T]] =
    val tpe = TypeRepr.of[T]
    val parents = parentsOf[T]
    val methods = MockableDefinitions(tpe)
    val log = Expr.summon[CallLog]

    val classSymbol = Symbol.newClass(
      parent = Symbol.spliceOwner,
      name = "anon",
      parents = parents.map {
        case term: Term => term.tpe
        case tpt: TypeTree => tpt.tpe
      },
      decls = classSymbol =>
        methods.flatMap { method =>
          List(
            Symbol.newVal(
              parent = classSymbol,
              name = method.stubValName,
              tpe = TypeRepr.of[StubbedMethod.Internal[Any, Any]],
              flags = Flags.EmptyFlags,
              privateWithin = Symbol.noSymbol
            ),
            if method.symbol.isValDef then
              Symbol.newVal(
                parent = classSymbol,
                name = method.symbol.name,
                tpe = method.tpeWithSubstitutedInnerTypesFor(classSymbol),
                flags = Flags.Override,
                privateWithin = Symbol.noSymbol
              )
            else
              Symbol.newMethod(
                parent = classSymbol,
                name = method.symbol.name,
                tpe = method.tpeWithSubstitutedInnerTypesFor(classSymbol),
                flags = Flags.Override,
                privateWithin = Symbol.noSymbol
              ),
          )
        } ::: List(
          Some(
            Symbol.newMethod(
              parent = classSymbol,
              name = ClearStubsMethodName,
              tpe = TypeRepr.of[Unit],
              flags = Flags.EmptyFlags,
              privateWithin = Symbol.noSymbol
            )
          ),
          log.map { _ =>
            Symbol.newVal(
              parent = classSymbol,
              name = UniqueStubIdx,
              tpe = TypeRepr.of[Int],
              flags = Flags.EmptyFlags,
              privateWithin = Symbol.noSymbol
            )
          }
        ).flatten,
      selfType = None
    )

    val classDef = ClassDef(
      cls = classSymbol,
      parents = parents,
      body = methods.flatMap { method =>
        List(
          ValDef(
            classSymbol.declaredField(method.stubValName),
            Some {
              '{ new StubbedMethod.Internal[Any, Any] }.asTerm
            }
          ),
          if (method.symbol.isValDef)
            ValDef(method.symbol.overridingSymbol(classSymbol), Some('{ null }.asTerm))
          else
            DefDef(
              symbol = method.symbol.overridingSymbol(classSymbol),
              params => Some {
                val resTpe = method.tpe.prepareResType(method.resTypeWithInnerTypesOverrideFor(classSymbol), params)
                resTpe.asType match
                  case '[res] =>
                    val ioOpt = Expr.summon[StubIO[?]].filter(_.isMatches(resTpe))
                    val tupledArgs = tupled(params.flatMap {_.collect { case term: Term => term }}).asExpr
                    '{ ${ method.stubApiRef(classSymbol) }.impl(${tupledArgs}).asInstanceOf[res] }
                      .asTerm
                      .changeOwner(method.symbol.overridingSymbol(classSymbol))
              }
            )
        )
      } :::
        List(
          Some(
            DefDef(
              symbol = classSymbol.methodMember(ClearStubsMethodName).head,
              _ =>
                Some(
                  Block(
                    methods.map { method => '{ ${ method.stubApiRef(classSymbol) }.clear() }.asTerm },
                    '{}.asTerm
                  )
                )
            )
          ),
          log.map { log =>
            ValDef(
              symbol = classSymbol.declaredField(UniqueStubIdx),
              Some {
                '{${log}.internal.nextIdx}.asTerm
              }
            )
          }
        ).flatten
    )

    val instance = Block(
      List(classDef),
      Typed(
        Apply(
          Select(New(TypeIdent(classSymbol)), classSymbol.primaryConstructor),
          Nil
        ),
        TypeTree.of[T & scala.reflect.Selectable]
      )
    )
    '{
      ${ collector }.bind(${ instance.asExprOf[T] }.asInstanceOf[Stub[T]])
    }
  end newInstance

  @experimental
  def parseMethods(calls: Expr[Seq[Any]]): Expr[List[String]] =
    Expr.ofList {
      parseCalls(calls.asTerm).map {
        case select: Select =>
          searchTermWithMethod(select, Nil).show

        case Lambda(paramClause, term) =>
          searchTermWithMethod(term, paramClause.map(_.tpt.tpe)).show

        case Inlined(_, _, Lambda(paramClause, term)) =>
          searchTermWithMethod(term, paramClause.map(_.tpt.tpe)).show

        case tree =>
          report.errorAndAbort(s"Not a method selection: ${tree.show(using Printer.TreeStructure)}")
      }
    }

  @tailrec
  private def parseCalls(term: Term): List[Term] =
    term match
      case Inlined(_, _, term) =>
        parseCalls(term)

      case Typed(Repeated(List(term: Inlined), _), _) =>
        parseCalls(term)

      case Typed(Repeated(lambdas, _), _) =>
        lambdas

      case other =>
        report.errorAndAbort(s"Unknown tree: ${other.show(using Printer.TreeStructure)}")


  @tailrec
  private def tupleTypeToList(tpe: TypeRepr,
                              acc: mutable.ListBuffer[TypeRepr] = new ListBuffer[TypeRepr]): List[TypeRepr] =
    tpe.asType match
      case '[h *: t] =>
        tupleTypeToList(TypeRepr.of[t], acc += TypeRepr.of[h])
      case '[EmptyTuple] =>
        acc.toList
      case _ =>
        (acc += tpe).toList


  @experimental
  def getStubbed0[R: Type](select: Expr[R]): Expr[StubbedMethod[Unit, R]] =
    searchTermWithMethod(select.asTerm, Nil).selectReflect[StubbedMethod[Unit, R]](_.stubValName)

  @experimental
  def getStubbed1[T1: Type, R: Type](select: Expr[T1 => R]): Expr[StubbedMethod[T1, R]] =
    searchTermWithMethod(select.asTerm, List(TypeRepr.of[T1])).selectReflect[StubbedMethod[T1, R]](_.stubValName)

  @experimental
  def getStubbed[Args <: NonEmptyTuple: Type, R: Type](select: Expr[Args => R]): Expr[StubbedMethod[Args, R]] =
    searchTermWithMethod(select.asTerm, tupleTypeToList(TypeRepr.of[Args])).selectReflect[StubbedMethod[Args, R]](_.stubValName)

  private def tupled(args: List[Term]): Term =
    args match
      case Nil => '{()}.asTerm
      case oneArg :: Nil => oneArg
      case args =>
        args.foldRight[Term]('{ EmptyTuple }.asTerm) { (el, acc) =>
          Select
            .unique(acc, "*:")
            .appliedToTypes(List(el.tpe, acc.tpe))
            .appliedToArgs(List(el))
        }

  extension (value: StubWithMethod)
    def show: Expr[String] = '{
      val idx = ${value.selectReflect[Int](_ => UniqueStubIdx)}
      val method = ${Expr(value.method.show)}
      s"<stub-$idx> $method"
    }


  extension (method: MockableDefinition)
    private def stubApiRef(classSymbol: Symbol): Expr[StubbedMethod.Internal[Any, Any]] =
      Ref(classSymbol.declaredField(method.stubValName)).asExprOf[StubbedMethod.Internal[Any, Any]]

    private def show: String =
      given Printer[TypeRepr] = Printer.TypeReprShortCode
      s"Stub[${method.ownerTpe.show}].${method.symbol.name}${method.tpe.show}"


  extension (io: Expr[StubIO[?]])
    private def isMatches(resTpe: TypeRepr): Boolean =
      val ioTerm = io.asTerm
      val ioTpe = ioTerm.tpe.select(ioTerm.tpe.typeSymbol.typeMember("Underlying"))
      resTpe <:< AppliedType(ioTpe, List(TypeRepr.of[Any], TypeRepr.of[Any]))

    private def wrap(resTpe: TypeRepr, updateCalls: Expr[Unit], result: Expr[Any]): Term =
      val ioTerm = io.asTerm
      val (errorArgTpe, resArgTpe) = resTpe.dealias.typeArgs match
        case List(_, err, res) => (err, res)
        case List(err, res) => (err, res)
        case List(res) => (TypeRepr.of[Nothing], res)
        case _ => report.errorAndAbort(s"$resTpe is not a type constructor")

      resTpe.asType match
        case '[res] =>
          TypeApply(
            Select.unique(
              Apply(
                Apply(
                  TypeApply(
                    Select.unique(ioTerm, "flatMap"),
                    List(
                      Inferred(TypeRepr.of[Nothing]),
                      Inferred(errorArgTpe),
                      Inferred(TypeRepr.of[Unit]),
                      Inferred(resArgTpe)
                    )
                  ),
                  List(
                    Apply(
                      TypeApply(Select.unique(ioTerm, "succeed"), List(TypeTree.of[Unit])),
                      List(updateCalls.asTerm)
                    )
                  )
                ),
                List('{ (_: Unit) => ${ result }.asInstanceOf[res] }.asTerm)
              ),
              "asInstanceOf"
            ),
            List(TypeTree.of[res])
          )
