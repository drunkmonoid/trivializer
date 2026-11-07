package trivializer

// One sequence that contains each element of the ambient set once, hence one bijection.

object Permutation:
  opaque type Permutation[A] = (FiniteSet.Set[A], Sequence.Sequence[A])

  private def fromList[A](items: List[A]): Sequence.Sequence[A] =
    items.foldRight(Sequence.empty[A])(Sequence.cons)

  def tryOf[A: Ordering](
      ambient: FiniteSet.Set[A],
      order: Sequence.Sequence[A]
  ): Option[Permutation[A]] =
    val seen = order.toList.foldLeft(FiniteSet.empty[A])((acc, item) => acc.insert(item))
    if order.length == ambient.cardinality && seen.equal(ambient) then Some((ambient, order))
    else None

  private def imageOf[A: Ordering](permutation: Permutation[A], element: A): Option[A] =
    val (ambient, order): (FiniteSet.Set[A], Sequence.Sequence[A]) = permutation
    val index = ambient.toList.indexWhere(item => summon[Ordering[A]].equiv(item, element))
    if index < 0 then None else order.item(index)

  extension [A: Ordering](permutation: Permutation[A])
    def ambient: FiniteSet.Set[A] =
      val (set, _): (FiniteSet.Set[A], Sequence.Sequence[A]) = permutation
      set

    def sequence: Sequence.Sequence[A] =
      val (_, order): (FiniteSet.Set[A], Sequence.Sequence[A]) = permutation
      order

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
