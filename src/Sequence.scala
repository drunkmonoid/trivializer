package trivializer

// A sequence keeps order and allows repetition, so it is not a set.

object Sequence:
  opaque type Sequence[A] = List[A]

  def empty[A]: Sequence[A] = Nil

  def cons[A](head: A, tail: Sequence[A]): Sequence[A] = head :: tail

  given [A: Ordering]: Ordering[Sequence[A]] with
    def compare(left: Sequence[A], right: Sequence[A]): Int =
      def walk(xs: List[A], ys: List[A]): Int =
        (xs, ys) match
          case (Nil, Nil) => 0
          case (Nil, _)   => -1
          case (_, Nil)   => 1
          case (x :: xt, y :: yt) =>
            val here = summon[Ordering[A]].compare(x, y)
            if here != 0 then here else walk(xt, yt)

      val xs: List[A] = left
      val ys: List[A] = right
      walk(xs, ys)

  private def append[A](sequence: Sequence[A], last: A): Sequence[A] =
    val items: List[A] = sequence
    items :+ last

  // The set of length-k words, built by products. The size is how many were stored.
  def words[A: Ordering](alphabet: FiniteSet.Set[A], k: Int): FiniteSet.Set[Sequence[A]] =
    if k < 0 then FiniteSet.empty
    else if k == 0 then FiniteSet.singleton(empty[A])
    else
      val shorter = words(alphabet, k - 1)
      val pairs = FiniteSet.product(shorter, alphabet)
      FiniteSet.map((pair: (Sequence[A], A)) => append(pair._1, pair._2), pairs)

  extension [A](sequence: Sequence[A])
    def item(index: Int): Option[A] =
      val items: List[A] = sequence
      if index < 0 then None else items.lift(index)

    def length: Int =
      val items: List[A] = sequence
      items.length

    def toList: List[A] = sequence
