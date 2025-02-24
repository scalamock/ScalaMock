package newapi

import org.scalamock.stubs.{CallLog, ZIOStubs}
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}
import zio.{IO, Scope, UIO, ZIO}

object ZIOVerifySpec extends ZIOSpecDefault with ZIOStubs {
  trait FirstTrait {
    def foo(x: Int, y: Int): UIO[Int]
    def foo2(x: Int): UIO[Int]
  }

  trait SecondTrait {
    def bar(x: String): IO[String, String]
  }

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("zio verify test-cases")(
      test("verify") {
        implicit val log: CallLog = CallLog()
        val first = stub[FirstTrait]
        val second = stub[SecondTrait]
        for {
          _ <- (first.foo _).returnsZIO(_ => ZIO.succeed(0))
          _ <- (first.foo2 _).returnsZIO(_ => ZIO.succeed(0))
          _ <- (second.bar _).returnsZIO(_ => ZIO.succeed(""))
          _ <- second.bar("1")
          _ <- first.foo(1, 1)
          _ <- first.foo2(1)
        } yield assertTrue(
          (second.bar _).isBefore(first.foo _),
          (second.bar _).calls == List("1")
        )
      })
}
