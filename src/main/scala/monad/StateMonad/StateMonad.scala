package monad.state

object StateMonad extends App {
  type Stack[A] = List[A]

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = State(s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      val fn = f(a)
      fn.run(s1)
    })
  }

  def push[A](a: A): State[Stack[A], Unit] = State(s => ((), a :: s))

  def pop[A]: State[Stack[A], Option[A]] = State({
    case head :: tail => (Some(head), tail)
    case Nil => (None, Nil)
  })

  def popPairs[A]: State[Stack[A], (Option[A], Option[A])] =
    pop[A].flatMap(pop1 => pop[A].map(pop2 => (pop1, pop2)))

  val initStack = List(1,2,3,4)

  println(popPairs.run(initStack))

  push(2).run(List(5,6,7))
}
