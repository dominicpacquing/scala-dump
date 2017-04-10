package scaladump.monad.reader

import scalaz._
import Scalaz._

case class User(id: Int, name: String, age: Int, email: String)

trait UserRepository {
  def find(id: Int): Option[User] =
    Some(User(id = 1, name = "John", email = "john@gmail.com", age = 22))
}

object UserInfo {
  def email(id: Int): Reader[UserRepository, Option[String]] =
    Reader( (userRepository: UserRepository) => userRepository.find(id).map(_.email) )

  def name(id: Int): Reader[UserRepository, Option[String]] =
    Reader( (userRepository: UserRepository) => userRepository.find(id).map(_.name) )

  // UserRepository => Map[String, Option[String]]
  def details(id: Int): Reader[UserRepository, Map[String, Option[String]]] =
    email(id).flatMap { opStringEmail =>
      name(id).map { opStringName =>
        Map("name" -> opStringName, "email" -> opStringEmail)
      }
    }
}

object ReaderMonad extends App {
  val readerMonad = UserInfo.details(1)
  val output = readerMonad.run(new UserRepository{})
  println(output)
}
