package trivializer

@main def run(): Unit =
  def check(name: String, ok: Boolean): Boolean =
    println(s"$name: $ok")
    ok

  val stored =
    FiniteSet
      .empty[Int]
      .insert(2)
      .insert(1)
      .insert(2)
      .insert(0)

  val elements = stored.toList

  println("finite set")
  println(s"stored: $elements")

  val removed = stored.remove(1)

  val reordered =
    FiniteSet.empty[Int].insert(0).insert(2).insert(1)

  val smaller = stored.remove(2)

  val left = FiniteSet.empty[Int].insert(0).insert(1)
  val right = FiniteSet.empty[Int].insert(1).insert(2)

  val results = List(
    check("stored elements", elements == List(0, 1, 2)),
    check("membership", stored.contains(1) && !stored.contains(3)),
    check("singleton", FiniteSet.singleton(4).toList == List(4)),
    check("cardinality", FiniteSet.empty[Int].cardinality == 0 && stored.cardinality == 3),
    check(
      "removal",
      removed.toList == List(0, 2) && stored.remove(9).toList == elements
    ),
    check("equality", stored.equal(reordered) && !stored.equal(smaller)),
    check(
      "subset",
      smaller.isSubset(stored) && FiniteSet.empty[Int].isSubset(stored) && !stored.isSubset(smaller)
    ),
    check(
      "boolean",
      left.union(right).toList == List(0, 1, 2)
        && left.intersect(right).toList == List(1)
        && left.difference(right).toList == List(0)
        && right.difference(left).toList == List(2)
        && left.union(FiniteSet.empty[Int]).equal(left)
    ),
    check("filter", stored.filter(x => x % 2 == 0).toList == List(0, 2)),
    check(
      "map",
      stored.map(x => x % 2).toList == List(0, 1)
        && stored.map(_ => 0).cardinality == 1
        && stored.map(x => x + 1).toList == List(1, 2, 3)
    ),
    check(
      "product", {
        val pairs = FiniteSet.product(left, right)
        pairs.toList == List((0, 1), (0, 2), (1, 1), (1, 2))
          && pairs.cardinality == pairs.toList.length
          && FiniteSet.product(left, FiniteSet.empty[Int]).cardinality == 0
          && FiniteSet.fst((0, 1)) == 0
          && FiniteSet.snd((0, 1)) == 1
      }
    ),
    check(
      "sum", {
        val tagged = FiniteSet.sum(left, right)
        tagged.toList == List(
          FiniteSet.injectLeft[Int, Int](0),
          FiniteSet.injectLeft[Int, Int](1),
          FiniteSet.injectRight[Int, Int](1),
          FiniteSet.injectRight[Int, Int](2)
        )
          && tagged.cardinality == 4
          && tagged.contains(FiniteSet.injectLeft[Int, Int](1))
          && tagged.contains(FiniteSet.injectRight[Int, Int](1))
      }
    ),
    check(
      "relation", {
        val related =
          Relation
            .empty[Int, Int]
            .add(0, 1)
            .add(0, 2)
            .add(1, 1)
        related.domain.toList == List(0, 1)
          && related.range.toList == List(1, 2)
          && related.pairs.cardinality == 3
      }
    ),
    check(
      "function", {
        val single = Relation.empty[Int, Int].add(0, 1).add(1, 2)
        val branched = Relation.empty[Int, Int].add(0, 1).add(0, 2)
        val id = Function.identity(stored)
        (Function.tryOfRelation(single), Function.tryOfRelation(branched)) match
          case (Some(f), None) =>
            f(0) == Some(1)
              && f(1) == Some(2)
              && f(3) == None
              && id(1) == Some(1)
              && id(9) == None
          case _ => false
      }
    ),
    check(
      "compose", {
        val before = Relation.empty[Int, Int].add(0, 1).add(1, 2)
        val after = Relation.empty[Int, Int].add(1, 10).add(2, 20)
        (Function.tryOfRelation(before), Function.tryOfRelation(after)) match
          case (Some(beforeF), Some(afterF)) =>
            val chained = afterF.compose(beforeF) match
              case Some(composed) => composed(0) == Some(10) && composed(1) == Some(20)
              case None           => false
            val same = Function.identity(left)
            val identityComposed = same.compose(same) match
              case Some(composed) => composed(0) == Some(0) && composed(1) == Some(1)
              case None           => false
            chained && identityComposed && beforeF.compose(afterF) == None
          case _ => false
      }
    ),
    check(
      "kinds", {
        val swap = Relation.empty[Int, Int].add(0, 1).add(1, 0)
        val collapse = Relation.empty[Int, Int].add(0, 1).add(1, 1)
        val domain = FiniteSet.empty[Int].insert(0).insert(1)
        val images = FiniteSet.empty[Int].insert(0).insert(1)
        val bigger = images.insert(2)
        (Function.tryOfRelation(swap), Function.tryOfRelation(collapse)) match
          case (Some(bijection), Some(constant)) =>
            bijection.isBijective(domain, images)
              && bijection.isInjective(domain, bigger)
              && !bijection.isSurjective(domain, bigger)
              && constant.isSurjective(domain, FiniteSet.singleton(1))
              && !constant.isInjective(domain, FiniteSet.singleton(1))
              && !constant.isBijective(domain, images)
          case _ => false
      }
    ),
    check(
      "sequence", {
        val repeated = Sequence.cons(2, Sequence.cons(1, Sequence.cons(2, Sequence.empty)))
        repeated.toList == List(2, 1, 2)
          && repeated.length == 3
          && repeated.item(0) == Some(2)
          && repeated.item(1) == Some(1)
          && repeated.item(3) == None
          && Sequence.empty[Int].length == 0
      }
    )
  )

  if !results.forall(identity) then sys.exit(1)
