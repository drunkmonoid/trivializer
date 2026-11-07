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

  def injections[A: Ordering, B: Ordering](
      domain: FiniteSet.Set[A],
      codomain: FiniteSet.Set[B]
  ): FiniteSet.Set[Function.Function[A, B]] =
    functions(domain, codomain).filter(_.isInjective(domain, codomain))

  def surjections[A: Ordering, B: Ordering](
      domain: FiniteSet.Set[A],
      codomain: FiniteSet.Set[B]
  ): FiniteSet.Set[Function.Function[A, B]] =
    functions(domain, codomain).filter(_.isSurjective(domain, codomain))

  def bijections[A: Ordering, B: Ordering](
      domain: FiniteSet.Set[A],
      codomain: FiniteSet.Set[B]
  ): FiniteSet.Set[Function.Function[A, B]] =
    functions(domain, codomain).filter(_.isBijective(domain, codomain))

  def count[A](family: FiniteSet.Set[A]): Int = family.cardinality

  def subsetsBySize[A: Ordering](
      set: FiniteSet.Set[A]
  ): FiniteSet.Set[(Int, FiniteSet.Set[A])] =
    def gather(k: Int, acc: FiniteSet.Set[(Int, FiniteSet.Set[A])]): FiniteSet.Set[(Int, FiniteSet.Set[A])] =
      if k > set.cardinality then acc
      else
        val tagged = set.kSubsets(k).map(subset => (k, subset))
        gather(k + 1, acc.union(tagged))

    gather(0, FiniteSet.empty)

  def complement[A: Ordering](
      ambient: FiniteSet.Set[A],
      k: Int
  ): Function.Function[FiniteSet.Set[A], FiniteSet.Set[A]] =
    val pairs = ambient.kSubsets(k).toList.foldLeft(FiniteSet.empty[(FiniteSet.Set[A], FiniteSet.Set[A])]) {
      (acc, subset) => acc.insert((subset, ambient.difference(subset)))
    }
    Function.tryOfRelation(Relation.ofPairs(pairs)).get

  def subsetToWord(
      ambient: FiniteSet.Set[Int]
  ): Function.Function[FiniteSet.Set[Int], Sequence.Sequence[Int]] =
    val pairs =
      ambient.power.toList.foldLeft(FiniteSet.empty[(FiniteSet.Set[Int], Sequence.Sequence[Int])]) {
        (acc, subset) =>
          val letters = ambient.toList.map(element => if subset.contains(element) then 1 else 0)
          val word = letters.foldRight(Sequence.empty[Int])(Sequence.cons)
          acc.insert((subset, word))
      }
    Function.tryOfRelation(Relation.ofPairs(pairs)).get

  def wordToSubset(ambient: FiniteSet.Set[Int], word: Sequence.Sequence[Int]): FiniteSet.Set[Int] =
    ambient.toList.zip(word.toList).foldLeft(FiniteSet.empty[Int]) { (acc, pair) =>
      if pair._2 == 1 then acc.insert(pair._1) else acc
    }
