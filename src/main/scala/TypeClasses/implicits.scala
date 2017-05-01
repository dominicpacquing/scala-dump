case class Person(name: String, age: Int) {
  def greet: String = s"Hello ${name}"
}

object Person {
  implicit val person: Person = Person("Paul", 29)
  implicit val somePerson: Option[Person] = Some(Person("SomePaul", 29))
}

object ImplicitsResolutionConversion extends App {
  def sayHello(implicit person: Person): String = {
    person.name
  }

  def sayHelloSome(implicit personSome: Option[Person]): String = {
    personSome match {
      case Some(person) => person.name
      case None => ""
    }
  }

  println(sayHello)
  println(sayHelloSome)
}
