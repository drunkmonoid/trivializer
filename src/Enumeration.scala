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

  def setPartitions[A: Ordering](
      ambient: FiniteSet.Set[A]
  ): FiniteSet.Set[Partition.Partition[A]] =
    def grow(
        remaining: List[A],
        blocks: List[FiniteSet.Set[A]]
    ): FiniteSet.Set[Partition.Partition[A]] =
      remaining match
        case Nil =>
          val stored =
            blocks.foldLeft(FiniteSet.empty[FiniteSet.Set[A]])((acc, block) => acc.insert(block))
          Partition.tryOf(ambient, stored) match
            case Some(partition) => FiniteSet.singleton(partition)
            case None            => FiniteSet.empty
        case element :: rest =>
          val intoExisting =
            blocks.zipWithIndex.foldLeft(FiniteSet.empty[Partition.Partition[A]]) { (acc, indexed) =>
              val (block, index) = indexed
              val next = blocks.updated(index, block.insert(element))
              acc.union(grow(rest, next))
            }
          intoExisting.union(grow(rest, blocks :+ FiniteSet.singleton(element)))

    grow(ambient.toList, Nil)

  def functions[A: Ordering, B: Ordering](
      domain: FiniteSet.Set[A],
      codomain: FiniteSet.Set[B]
  ): FiniteSet.Set[Function.Function[A, B]] =
    def assign(
        remaining: List[A],
        pairs: FiniteSet.Set[(A, B)]
    ): FiniteSet.Set[Function.Function[A, B]] =
      remaining match
        case Nil =>
          Function.tryOfRelation(Relation.ofPairs(pairs)) match
            case Some(function) => FiniteSet.singleton(function)
            case None           => FiniteSet.empty
        case element :: rest =>
          codomain.toList.foldLeft(FiniteSet.empty[Function.Function[A, B]]) { (acc, image) =>
            acc.union(assign(rest, pairs.insert((element, image))))
          }

    assign(domain.toList, FiniteSet.empty)
