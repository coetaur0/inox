package inox

/** A compilation result. */
enum Result[Item, Error]:
  case Success(item: Item)
  case Failure(errors: IndexedSeq[Error])

  /** Returns `true` if the result is a success. */
  def isSuccess: Boolean =
    this match
      case Success(_) => true
      case Failure(_) => false

  def map[B](f: Item => B): Result[B, Error] =
    this match
      case Success(item)   => Success(f(item))
      case Failure(errors) => Failure(errors)

  def flatMap[B](f: Item => Result[B, Error]): Result[B, Error] =
    this match
      case Success(item)   => f(item)
      case Failure(errors) => Failure(errors)
