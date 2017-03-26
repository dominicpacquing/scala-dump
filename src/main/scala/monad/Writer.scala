package scaladump.monad.writer

import scalaz._
import Scalaz._

case class Person(name: String, income: Double)

object ATO {
  val tax = 0.3
  def addTax(amount: Double): Double = amount * tax + amount

  val penalty = .09
  def addPenalty(amount: Double): Double = amount * penalty + amount
}

object WriterMonad extends App {

  val john = Person(name = "John", income = 120)

  val incomeWithPenaltyWriter: Writer[List[String], Double] = for {
    incomeWithTax     <- ATO.addTax(john.income)       set s"Added ${john.name} tax" :: Nil
    incomeWithPenalty <- ATO.addPenalty(incomeWithTax) set s"Added penalty" :: Nil
  } yield (incomeWithPenalty)

  val (logs, totalS) = incomeWithPenaltyWriter.run

  println(logs)
  println(totalS)
}
