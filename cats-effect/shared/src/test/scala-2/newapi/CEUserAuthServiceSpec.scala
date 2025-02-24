package newapi

import cats.effect.IO
import munit.CatsEffectSuite
import org.scalamock.stubs._

sealed trait UserStatus

object UserStatus {
  case object Normal extends UserStatus
  case object Blocked extends UserStatus
}

sealed trait FailedAuthResult

object FailedAuthResult {
  case object UserNotFound extends FailedAuthResult
  case object UserNotAllowed extends FailedAuthResult
  case object WrongPassword extends FailedAuthResult
}

case class User(id: Long, status: UserStatus)

trait UserService {
  def findUser(userId: Long): IO[Option[User]]
}

trait PasswordService {
  def checkPassword(id: Long, password: String): IO[Boolean]
}

class UserAuthService(
  userService: UserService,
  passwordService: PasswordService
) {
  def authorize(id: Long, password: String): IO[Either[FailedAuthResult, Unit]] =
    userService.findUser(id).flatMap {
      case None =>
        IO(Left(FailedAuthResult.UserNotFound))

      case Some(user) if user.status == UserStatus.Blocked =>
        IO(Left(FailedAuthResult.UserNotAllowed))

      case Some(user) =>
        passwordService.checkPassword(id, password).map {
          case true => Right(())
          case false => Left(FailedAuthResult.WrongPassword)
        }
    }
}


class CEUserAuthServiceSpec extends CatsEffectSuite with CatsEffectStubs {
  val unknownUserId = 0
  val user = User(1, UserStatus.Normal)
  val blockedUser = User(2, UserStatus.Blocked)
  val validPassword = "valid"
  val invalidPassword = "invalid"

  case class Verify(
                     passwordCheckedTimes: Option[Int]
                   )

  testCase(
    description = "error if user not found",
    id = unknownUserId,
    password = validPassword,
    expectedResult = Left(FailedAuthResult.UserNotFound),
    verify = Verify(passwordCheckedTimes = Some(0))
  )

  testCase(
    description = "error if user is blocked",
    id = blockedUser.id,
    password = validPassword,
    expectedResult = Left(FailedAuthResult.UserNotAllowed),
    verify = Verify(passwordCheckedTimes = Some(0))
  )

  testCase(
    description = "error if password is invalid",
    id = user.id,
    password = invalidPassword,
    expectedResult = Left(FailedAuthResult.WrongPassword),
    verify = Verify(passwordCheckedTimes = Some(1))
  )

  testCase(
    description = "password valid",
    id = user.id,
    password = validPassword,
    expectedResult = Right(()),
    verify = Verify(passwordCheckedTimes = Some(1))
  )

  def testCase(
                description: String,
                id: Long,
                password: String,
                expectedResult: Either[FailedAuthResult, Unit],
                verify: Verify
              ) = test(description) {
    val userService = stub[UserService]
    val passwordService = stub[PasswordService]
    val userAuthService = new UserAuthService(userService, passwordService)
    val result =
      for {
        _ <- (userService.findUser _).returnsIO {
          case user.id => IO.some(user)
          case blockedUser.id => IO.some(blockedUser)
          case _ => IO.none
        }

        _ <- (passwordService.checkPassword _).returnsIO {
          case (_, password) => IO(password == validPassword)
        }

        result <- userAuthService.authorize(id, password)

      } yield (result, Option((passwordService.checkPassword _).times))

    assertIO(result, (expectedResult, verify.passwordCheckedTimes))
  }
}