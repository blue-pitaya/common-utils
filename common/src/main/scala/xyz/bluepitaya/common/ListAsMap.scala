package xyz.bluepitaya.common

object ListAsMap {
  implicit class ListAsMapExtension[A, B <: Identified[A]](xs: List[B]) {
    def get(id: A): Option[B] = xs.find(x => x.id == id)

    def adjust(obj: B): Option[List[B]] = for {
      _ <- xs.find(x => x.id == obj.id)
      updated = xs.map(x => if (x.id == obj.id) obj else x)
    } yield (updated)

    def removed(id: A): List[B] = xs.filter(x => x.id != id)

    def updatedById(id: A, obj: B): List[B] = xs
      .map(x => if (x.id == id) obj else x)

    def moveToBackList(obj: B): Option[List[B]] = for {
      x <- xs.find(o => o.id == obj.id)
      rest = xs.filter(o => o.id != obj.id)
    } yield (rest :+ x)
  }
}
