package trivializer

// A sequence keeps order and allows repetition, so it is not a set.

object Sequence:
  opaque type Sequence[A] = List[A]

  def empty[A]: Sequence[A] = Nil

  def cons[A](head: A, tail: Sequence[A]): Sequence[A] = head :: tail

  extension [A](sequence: Sequence[A])
    def item(index: Int): Option[A] =
      val items: List[A] = sequence
      if index < 0 then None else items.lift(index)

    def length: Int =
      val items: List[A] = sequence
      items.length

    def toList: List[A] = sequence
