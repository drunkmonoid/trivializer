package trivializer

// A witnessed subset is an ambient set plus the chosen elements.
// Construction succeeds only when every chosen element lies in the ambient set.

object Subset:
  opaque type Subset[A] = (FiniteSet.Set[A], FiniteSet.Set[A])

  def tryWitness[A: Ordering](
      ambient: FiniteSet.Set[A],
      chosen: FiniteSet.Set[A]
  ): Option[Subset[A]] =
    if chosen.isSubset(ambient) then Some((ambient, chosen)) else None

  extension [A](subset: Subset[A])
    def ambient: FiniteSet.Set[A] =
      val pair: (FiniteSet.Set[A], FiniteSet.Set[A]) = subset
      pair._1

    def chosen: FiniteSet.Set[A] =
      val pair: (FiniteSet.Set[A], FiniteSet.Set[A]) = subset
      pair._2
