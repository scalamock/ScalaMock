package newapi

package tests

import cats.effect.IO
import munit.CatsEffectSuite
import org.scalamock.stubs.{CallLog, CatsEffectStubs}

class IOSpec extends CatsEffectSuite, CatsEffectStubs:

  trait Foo:
    def zeroArgsIO: IO[Option[String]]

    def oneArgIO(x: Int): IO[Option[String]]

    def twoArgsIO(x: Int, y: String): IO[Option[String]]

    def overloaded(x: Int, y: Boolean): IO[Int]

    def overloaded(x: String): IO[Boolean]

    def overloaded: IO[String]

    def typeArgsOptIO[A](value: A): IO[Option[A]]

    def typeArgsOptIOTwoParams[A](value: A, other: A): IO[Option[A]]

  trait FooBaz[F[_]] {
    def foo(x: Int): F[Int]
  }

  test("zero args"):
    given log: CallLog = CallLog()
    val foo = stub[Foo]
    val result = for
      _ <- foo.zeroArgsIO.returnsIO(IO.none[String])
      _ <- foo.zeroArgsIO
      _ <- foo.zeroArgsIO
    yield foo.zeroArgsIO.times

    assertIO(result, 2)

  test("one arg"):
    val foo = stub[Foo]
    val result = for
      _ <- foo.oneArgIO.returnsIO:
        case 1 => IO(None)
        case _ => IO(Some("1"))
      _ <- foo.oneArgIO(1)
      _ <- foo.oneArgIO(2)
    yield (foo.oneArgIO.times, foo.oneArgIO.calls)

    assertIO(result, (2, List(1, 2)))


  test("two args"):
    val foo = stub[Foo]
    val result =
      for
        _ <- foo.twoArgsIO.returnsIO:
          case (1, "foo") => IO(None)
          case _ => IO(Some("1"))
        _ <- foo.twoArgsIO(1, "foo")
        _ <- foo.twoArgsIO(2, "bar")
        times <- foo.twoArgsIO.timesIO
        calls <- foo.twoArgsIO.callsIO
      yield (times, calls)

    assertIO(result, (2, List((1, "foo"), (2, "bar"))))


  test("type args one param"):
    val foo = stub[Foo]
    val result = for
      _ <- foo.typeArgsOptIO[String].returnsIO:
        case "foo" => IO(Some("foo"))
        case _ => IO(None)
      result <- foo.typeArgsOptIO[String]("foo")
      times <- foo.typeArgsOptIO[String].timesIO
      calls <- foo.typeArgsOptIO[String].callsIO
    yield (result, times, calls)

    assertIO(result, (Some("foo"), 1, List("foo")))

  test("type args two params"):
    val foo = stub[Foo]
    val result = for
      _ <- foo.typeArgsOptIOTwoParams[Int].returnsIO:
        case (1, 2) => IO(Some(1))
        case _ => IO(None)
      result <- foo.typeArgsOptIOTwoParams[Int](1, 2)
      times <- foo.typeArgsOptIOTwoParams[Int].timesIO
      calls <- foo.typeArgsOptIOTwoParams[Int].callsIO
    yield (result, times, calls)

    assertIO(result, (Some(1), 1, List((1, 2))))

  test("mock interface with type constructors"):
    val foo = stub[FooBaz[IO]]
    val result = for
      _ <- foo.foo.returnsIO(_ => IO.pure(1))
      result <- foo.foo(1)
    yield result

    assertIO(result, 1)
