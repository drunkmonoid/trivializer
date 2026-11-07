package trivializer

// One sequence that contains each element of the ambient set once, hence one bijection.

object Permutation:
  opaque type Permutation[A] = (FiniteSet.Set[A], Sequence.Sequence[A])

  given [A: Ordering]: Ordering[Permutation[A]] with
    def compare(left: Permutation[A], right: Permutation[A]): Int =
      summon[Ordering[Sequence.Sequence[A]]].compare(left.sequence, right.sequence)

  private def fromList[A](items: List[A]): Sequence.Sequence[A] =
    items.foldRight(Sequence.empty[A])(Sequence.cons)

  def tryOf[A: Ordering](
      ambient: FiniteSet.Set[A],
      order: Sequence.Sequence[A]
  ): Option[Permutation[A]] =
    val seen = order.toList.foldLeft(FiniteSet.empty[A])((acc, item) => acc.insert(item))
    if order.length == ambient.cardinality && seen.equal(ambient) then Some((ambient, order))
    else None

  private def parts[A](
      permutation: Permutation[A]
  ): (FiniteSet.Set[A], Sequence.Sequence[A]) = permutation

  private def imageOf[A: Ordering](permutation: Permutation[A], element: A): Option[A] =
    val (ambient, order) = parts(permutation)
    val index = ambient.toList.indexWhere(item => summon[Ordering[A]].equiv(item, element))
    if index < 0 then None else order.item(index)

  extension [A: Ordering](permutation: Permutation[A])
    def ambient: FiniteSet.Set[A] =
      val (set, _) = parts(permutation)
      set

    def sequence: Sequence.Sequence[A] =
      val (_, order) = parts(permutation)
      order

    def at(element: A): Option[A] = imageOf(permutation, element)

    def act(elements: FiniteSet.Set[A]): Option[FiniteSet.Set[A]] =
      if !elements.isSubset(ambient) then None
      else
        Some(elements.toList.foldLeft(FiniteSet.empty[A]) { (acc, element) =>
          acc.insert(imageOf(permutation, element).get)
        })

    def orbit(element: A): Option[FiniteSet.Set[A]] =
      if imageOf(permutation, element).isEmpty then None
      else
        def walk(current: A, seen: FiniteSet.Set[A]): FiniteSet.Set[A] =
          if seen.contains(current) then seen
          else walk(imageOf(permutation, current).get, seen.insert(current))

        Some(walk(element, FiniteSet.empty[A]))

    def inverse: Permutation[A] =
      val elements = ambient.toList
      val preimages = elements.map { image =>
        elements
          .find { source =>
            imageOf(permutation, source).exists(found => summon[Ordering[A]].equiv(found, image))
          }
          .get
      }
      (ambient, fromList(preimages))

    // after ∘ before, when both permute the same ambient set.
    def compose(before: Permutation[A]): Option[Permutation[A]] =
      if !ambient.equal(before.ambient) then None
      else
        val images = ambient.toList.map { element =>
          imageOf(permutation, imageOf(before, element).get).get
        }
        Some((ambient, fromList(images)))
