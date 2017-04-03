package monad.statenoz

trait State[S, A] {
  def apply(s: S): (A, S)
  def map[B](f: A => B): State[S, B]
  def flatMap[B](f: A => State[S, B]): State[S, B]
}
