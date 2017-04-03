package monad.statenoz

/*
  Scala-style currying which is not elegant
*/
object StateNoZ extends App {

  type Stack[A] = List[A]

  def push[A](item: A)(stack: Stack[A]): Stack[A] = item :: stack

  def pop[A](stack: Stack[A]): (Option[A], Stack[A]) = stack match {
    case a :: tail => (Some(a), tail)
    case Nil => (None, Nil)
  }

  def popPairs[A](stack: Stack[A]): ((Option[A], Option[A]), Stack[A]) = {
    val (item, stack2) = pop(stack)
    val (item2, stack3) = pop(stack2)
    ((item, item2), stack3)
  }
}

/*
Haskell-like currying
*/
object FStateNoZ extends App {

  type Stack[A] = List[A]

  def push[A](item: A): Stack[A] => Stack[A] = stack => item :: stack

  def pop[A]: Stack[A] => (Option[A], Stack[A]) = stack => stack match {
    case a :: tail => (Some(a), tail)
    case Nil => (None, Nil)
  }

  def popPairs[A]: Stack[A] => ((Option[A], Option[A]), Stack[A]) = stack => {
    val (item, stack2) = pop(stack)
    val (item2, stack3) = pop(stack2)
    ((item, item2), stack3)
  }
}

/*
Refactor to follow pattern S => (A, S)
*/
object FStateNoZ2 extends App {
  type Stack[A] = List[A]

  def push[A](item: A): Stack[A] => (Unit, Stack[A]) = stack => ((), item :: stack)

  def pop[A]: Stack[A] => (Option[A], Stack[A]) = stack => stack match {
    case a :: tail => (Some(a), tail)
    case Nil => (None, Nil)
  }

  def popPairs[A]: Stack[A] => ((Option[A], Option[A]), Stack[A]) = stack => {
    val (item, stack2) = pop(stack)
    val (item2, stack3) = pop(stack2)
    ((item, item2), stack3)
  }
}

/*
Add type alias State[S, A] = S => (A, S)
*/
object FStateNoZ3 extends App {
  type State[S, A] = S => (A, S)
  type Stack[A] = List[A]

  def push[A](a: A): State[Stack[A], Unit] = stack => ((), a :: stack)

  def pop[A]: State[Stack[A], Option[A]] = stack => stack match {
    case a :: tail => (Some(a), tail)
    case Nil => (None, Nil)
  }

  def popPairs[A]: State[Stack[A], (Option[A], Option[A])] = stack => {
    val (item, stack2) = pop(stack)
    val (item2, stack3) = pop(stack2)
    ((item, item2), stack3)
  }
}

/*
Introduce map and flatMap
*/
object FStateNoZ4 extends App {
  type State[S, A] = S => (A, S)
  type Stack[A] = List[A]

  def map[S, A, B](sa: State[S, A])(f: A => B): State[S, B] = state => {
    val (newVal, newState) = sa(state)
    (f(newVal), newState)
  }

  def flatMap[S, A, B](sa: State[S, A])(f: A => State[S, B]): State[S, B] = state => {
    val (newVal, newState) = sa(state)
    val fn = f(newVal) // S => (B, S)
    fn(newState) // (B, S)
  } // S => (B, S)

  def push[A](a: A): State[Stack[A], Unit] = stack => ((), a :: stack)

  def pop[A]: State[Stack[A], Option[A]] = stack => stack match {
    case a :: tail => (Some(a), tail)
    case Nil => (None, Nil)
  }

  def popPairs[A]: State[Stack[A],(Option[A], Option[A])] =
    flatMap(pop[A])( op => map(pop[A])( op2 => (op, op2) ) )
}
