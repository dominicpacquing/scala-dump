import scalaz._, Scalaz._
import math.random

object ScalaZDisjunction extends App {
  def sampleNumber: Exception \/ Double = {
    val sampleNum = random * 100
    if (sampleNum >= 50)
      \/.right(sampleNum)
    else
      \/.left(new Exception("I do not like small number"))
  }

  val sN = sampleNumber

  println(sN)
  // ScalaZ disjunction is right - biased.
  println(sN.flatMap(x => \/-(x*2)))

  // I can also sequence ScalaZ disjunction
  println(List(sampleNumber, sampleNumber).sequenceU)
}
