package trivializer

// One witnessed subset whose cardinality is a given k.
// The value is that subset. The other k-subsets are not listed.

object Combination:
  opaque type Combination[A] = Subset.Subset[A]

  def tryOf[A: Ordering](
      ambient: FiniteSet.Set[A],
      chosen: FiniteSet.Set[A],
      k: Int
  ): Option[Combination[A]] =
    Subset.tryWitness(ambient, chosen) match
      case Some(witness) if witness.chosen.cardinality == k => Some(witness)
      case _                                                => None

  extension [A](combination: Combination[A])
    def subset: Subset.Subset[A] = combination
