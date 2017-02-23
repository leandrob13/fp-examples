package fp

import cats.data.{Ior, NonEmptyList}

/**
  * Created by leandrob13 on 2/22/17.
  */
package object ior {

  type IorNel[A, B] = Ior[NonEmptyList[A], NonEmptyList[B]]

  implicit class OptionOps[A](val o: Option[A]) extends AnyVal {

    def toIor[B](ifEmpty: => B): Ior[B, A] = o.fold[Ior[B, A]](Ior.left(ifEmpty))(Ior.right)
  }

  implicit class IorOps[A](val a: A) extends AnyVal {

    def toLeftIorNel[B]: IorNel[A, B] = Ior.left(NonEmptyList.of(a))

    def toRightIorNel[B]: IorNel[B, A] = Ior.right(NonEmptyList.of(a))
  }
}
