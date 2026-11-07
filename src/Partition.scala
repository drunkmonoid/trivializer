package trivializer

// A set of nonempty blocks. The blocks are pairwise disjoint and their union is the ambient set.

object Partition:
  opaque type Partition[A] = (FiniteSet.Set[A], FiniteSet.Set[FiniteSet.Set[A]])

  def tryOf[A: Ordering](
      ambient: FiniteSet.Set[A],
      blocks: FiniteSet.Set[FiniteSet.Set[A]]
  ): Option[Partition[A]] =
    val stored = blocks.toList
    val nonempty = stored.forall(_.cardinality > 0)
    val disjoint = stored.forall { left =>
      stored.forall { right =>
        left.equal(right) || left.intersect(right).cardinality == 0
      }
    }
    val covered = stored.foldLeft(FiniteSet.empty[A])((acc, block) => acc.union(block))
    if nonempty && disjoint && covered.equal(ambient) then Some((ambient, blocks)) else None

  extension [A](partition: Partition[A])
    def ambient: FiniteSet.Set[A] =
      val pair: (FiniteSet.Set[A], FiniteSet.Set[FiniteSet.Set[A]]) = partition
      pair._1

    def blocks: FiniteSet.Set[FiniteSet.Set[A]] =
      val pair: (FiniteSet.Set[A], FiniteSet.Set[FiniteSet.Set[A]]) = partition
      pair._2
