package trivializer

// Families of objects. Each family is stored. A later count is how many were stored.

object Enumeration:
  def permutations[A: Ordering](
      ambient: FiniteSet.Set[A]
  ): FiniteSet.Set[Permutation.Permutation[A]] =
    def place(
        remaining: FiniteSet.Set[A],
        built: List[A]
    ): FiniteSet.Set[Permutation.Permutation[A]] =
      if remaining.cardinality == 0 then
        val order = built.foldRight(Sequence.empty[A])(Sequence.cons)
        Permutation.tryOf(ambient, order) match
          case Some(permutation) => FiniteSet.singleton(permutation)
          case None              => FiniteSet.empty
      else
        remaining.toList.foldLeft(FiniteSet.empty[Permutation.Permutation[A]]) { (acc, element) =>
          acc.union(place(remaining.remove(element), built :+ element))
        }

    place(ambient, Nil)
