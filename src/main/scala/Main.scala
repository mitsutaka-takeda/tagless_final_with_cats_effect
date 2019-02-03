import cats._
import cats.effect._
import cats.syntax.functor._
import cats.syntax.flatMap._
import io.getquill._

import scala.concurrent.Future
import scala.util.{Failure, Random, Success}


object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = program.as(ExitCode.Success)

  case class UserId(value: String) extends AnyVal

  case class User(id: UserId, name: String)

  object User {
    def create(name: String): User = User(UserId(Random.alphanumeric.take(10).mkString), name)
  }

  trait UserRepository[F[_]] {
    def store(user: User): F[UserId]

    def find(id: UserId): F[Option[User]]
  }

  case class AccountId(value: String)

  case class Account(id: AccountId, email: String)

  object Account {
    def create(email: String): Account = Account(AccountId(Random.alphanumeric.take(10).mkString), email)
  }

  trait AccountRepository[F[_]] {
    def store(account: Account): F[AccountId]

    def find(id: AccountId): F[Option[Account]]
  }

  trait Console[F[_]] {
    def putStr(message: String): F[Unit]

    def readStr: F[String]
  }

  class StdConsole[F[_]: Sync] extends Console[F] {
    override def putStr(message: String): F[Unit] = Sync[F].delay(print(message))

    override def readStr: F[String] = Sync[F].delay(scala.io.StdIn.readLine())
  }

  object AccountCreateUseCase {
    def execute[F[_] : Monad](name: String, email: String)
                             (implicit U: UserRepository[F], A: AccountRepository[F]): F[String] = for {
      userId <- U.store(User.create(name))
      accountId <- A.store(Account.create(email))
    } yield s"$userId-$accountId"
  }

  class PostgresUserRepository[F[_] : Async] extends UserRepository[F] {
    // Need a Escape strategy to use the table name of 'user' in postgres.
    // https://getquill.io/#contexts-sql-contexts-naming-strategy
    lazy val ctx = new PostgresAsyncContext(NamingStrategy(SnakeCase, Escape), "ctx")

    import ctx._
    import scala.concurrent.ExecutionContext.Implicits._

    private def fromFuture[A](fa: F[Future[A]]): F[A] =
      fa.flatMap { future =>
        Async[F].async { cb =>
          future.onComplete {
            case Success(x) => cb(Right(x))
            case Failure(e) => cb(Left(e))
          }
        }
      }

    override def store(user: User): F[UserId] = {
      val a = quote {
        query[User].insert(lift(user))
      }
      fromFuture(Sync[F].delay(ctx.run(a).map(_ => user.id)))
    }

    override def find(id: UserId): F[Option[User]] = {
      val a = quote {
        query[User].filter(u => u.id == lift(id))
      }
      fromFuture(Sync[F].delay(ctx.run(a).map(_.headOption)))
    }
  }

  class MemoryAccountRepository[F[_] : Sync](accounts: scala.collection.mutable.Map[AccountId, Account]) extends AccountRepository[F] {
    override def store(Account: Account): F[AccountId] = for {
      _ <- Sync[F].delay(accounts += (Account.id -> Account))
    } yield Account.id

    override def find(id: AccountId): F[Option[Account]] = for {
      a <- Sync[F].delay(accounts.get(id))
    } yield a
  }

  class Controller[F[_] : Monad]
  (
    U: UserRepository[F],
    A: AccountRepository[F],
    C: Console[F]
  ) {
    def endpoint: F[Unit] = for {
      result <- AccountCreateUseCase.execute("a user", "test@test")(Monad[F], U, A)
      _ <- C.putStr(result)
    } yield ()
  }

  // Inject implementations
  lazy val program: IO[Unit] = new Controller[IO](
    new PostgresUserRepository[IO],
    new MemoryAccountRepository[IO](scala.collection.mutable.Map.empty),
    new StdConsole[IO]).endpoint
}
