package xyz.bluepitaya.common

object ListAsMap {
  implicit class ListAsMapExtension[A <: Identified](xs: List[A]) {
    def get(id: String): Option[A] = xs.find(x => x.id == id)

    def getOrError[E <: Throwable](
        id: String
    )(implicit notFoundError: String => E): Either[E, A] = get(id) match {
      case None        => Left(notFoundError(id))
      case Some(value) => Right(value)
    }

    def adjust(obj: A): Option[List[A]] = for {
      _ <- xs.find(x => x.id == obj.id)
      updated = xs.map(x => if (x.id == obj.id) obj else x)
    } yield (updated)

    def removed(id: String): List[A] = xs.filter(x => x.id != id)

    def updatedById(id: String, obj: A): List[A] = xs
      .map(x => if (x.id == id) obj else x)

    def moveToBackList(id: String): Option[List[A]] = for {
      x <- xs.find(o => o.id == id)
      rest = xs.filter(o => o.id != id)
    } yield (rest :+ x)

    def moveToBackListOrError[E <: Throwable](id: String)(implicit
        notFoundError: String => E
    ): Either[E, List[A]] = moveToBackList(id) match {
      case None        => Left(notFoundError(id))
      case Some(value) => Right(value)
    }
  }
}
