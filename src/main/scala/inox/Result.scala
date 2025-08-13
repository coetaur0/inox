package inox

import scala.collection.mutable

/** A compilation result. */
object Result {

  /** Builds a new result depending on the effect of some function `f`. */
  def build[Item, Error](
      f: mutable.Builder[Error, IndexedSeq[Error]] => Item
  ): Result[Item, Error] = {
    val errorBuilder = IndexedSeq.newBuilder[Error]
    val item = f(errorBuilder)
    if errorBuilder.result().isEmpty then Success(item)
    else Failure(errorBuilder.result())
  }

  /** Instantiates a new failure with a given error. */
  def fail[Item, Error](error: Error): Result[Item, Error] =
    Failure(IndexedSeq(error))

}

/** A compilation result. */
enum Result[Item, Error] {

  case Success(item: Item)
  case Failure(errors: IndexedSeq[Error])

  /** Returns `true` if the result is a success. */
  def isSuccess: Boolean =
    this match {
      case Success(_) => true
      case Failure(_) => false
    }

  def map[B](f: Item => B): Result[B, Error] =
    this match {
      case Success(item)   => Success(f(item))
      case Failure(errors) => Failure(errors)
    }

  def flatMap[B](f: Item => Result[B, Error]): Result[B, Error] =
    this match {
      case Success(item)   => f(item)
      case Failure(errors) => Failure(errors)
    }

  /** Applies some side effect function on the error values in a result if it is
    * a failure.
    */
  def handleFailure(f: IndexedSeq[Error] => Unit): Unit =
    this match {
      case Success(_)      => ()
      case Failure(errors) => f(errors)
    }

}
