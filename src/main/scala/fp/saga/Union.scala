package fp.saga

import cats.{ApplicativeError, Monad}

abstract class Union[U[_, _], A, B] extends ApplicativeError[U[A, ?], A] with Monad[U[A, ?]] {
  def isLeft: Boolean

  def isRight: Boolean

  def fold[C](Uab: U[A, B])(fl: A => C, fr: B => C): C

  def leftMap[C](Uab: U[A, B])(fl: A => C): U[C, B]
}
