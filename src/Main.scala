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
    ),
    check(
      "words", {
        val alphabet = FiniteSet.empty[Int].insert(0).insert(1)
        val found = Sequence.words(alphabet, 2)
        def word(first: Int, second: Int): Sequence.Sequence[Int] =
          Sequence.cons(first, Sequence.cons(second, Sequence.empty))
        found.toList == List(word(0, 0), word(0, 1), word(1, 0), word(1, 1))
          && found.cardinality == found.toList.length
          && !found.contains(word(0, 2))
          && Sequence.words(alphabet, 0).toList == List(Sequence.empty[Int])
          && Sequence.words(FiniteSet.empty[Int], 2).cardinality == 0
          && Sequence.words(FiniteSet.empty[Int], 0).cardinality == 1
      }
    ),
    check(
      "multiset", {
        val bag = Multiset.empty[Int].add(1).add(1).add(2)
        bag.multiplicity(1) == 2
          && bag.multiplicity(2) == 1
          && bag.multiplicity(3) == 0
          && bag.support.toList == List(1, 2)
          && bag.support.cardinality == 2
      }
    ),
    check(
      "witness", {
        val chosen = FiniteSet.empty[Int].insert(0).insert(1)
        Subset.tryWitness(stored, chosen) match
          case Some(witness) =>
            witness.chosen.toList == List(0, 1)
              && witness.ambient.equal(stored)
              && Subset.tryWitness(stored, FiniteSet.singleton(3)).isEmpty
              && Subset.tryWitness(stored, FiniteSet.empty[Int]).isDefined
          case None => false
      }
    ),
    check(
      "combination", {
        val chosen = FiniteSet.empty[Int].insert(0).insert(2)
        Combination.tryOf(stored, chosen, 2) match
          case Some(combination) =>
            combination.subset.chosen.equal(chosen)
              && combination.subset.chosen.cardinality == 2
              && Combination.tryOf(stored, chosen, 1).isEmpty
              && Combination.tryOf(stored, FiniteSet.singleton(3), 1).isEmpty
          case None => false
      }
    ),
    check(
      "permutation", {
        val cycle = Sequence.cons(1, Sequence.cons(2, Sequence.cons(0, Sequence.empty)))
        val repeated = Sequence.cons(1, Sequence.cons(1, Sequence.cons(0, Sequence.empty)))
        val identityOrder = Sequence.cons(0, Sequence.cons(1, Sequence.cons(2, Sequence.empty)))
        Permutation.tryOf(stored, cycle) match
          case Some(cyclePerm) =>
            val undone = cyclePerm.compose(cyclePerm.inverse)
            val squared = cyclePerm.compose(cyclePerm)
            undone.exists(_.sequence.toList == identityOrder.toList)
              && squared.exists(_.sequence.toList == List(2, 0, 1))
              && Permutation.tryOf(stored, repeated).isEmpty
          case None => false
      }
    ),
    check(
      "partition", {
        val alone = FiniteSet.singleton(0)
        val rest = FiniteSet.empty[Int].insert(1).insert(2)
        val blocks = FiniteSet.empty[FiniteSet.Set[Int]].insert(alone).insert(rest)
        val overlap = FiniteSet
          .empty[FiniteSet.Set[Int]]
          .insert(FiniteSet.empty[Int].insert(0).insert(1))
          .insert(FiniteSet.empty[Int].insert(1).insert(2))
        val gap = FiniteSet.empty[FiniteSet.Set[Int]].insert(alone).insert(FiniteSet.singleton(1))
        val emptyBlock = FiniteSet
          .empty[FiniteSet.Set[Int]]
          .insert(FiniteSet.empty[Int])
          .insert(stored)
        Partition.tryOf(stored, blocks) match
          case Some(partition) =>
            partition.blocks.cardinality == 2
              && partition.ambient.equal(stored)
              && Partition.tryOf(stored, overlap).isEmpty
              && Partition.tryOf(stored, gap).isEmpty
              && Partition.tryOf(stored, emptyBlock).isEmpty
              && Partition.tryOf(FiniteSet.empty[Int], FiniteSet.empty[FiniteSet.Set[Int]]).isDefined
          case None => false
      }
    ),
    check(
      "action", {
        val cycle = Sequence.cons(1, Sequence.cons(2, Sequence.cons(0, Sequence.empty)))
        val swap = Sequence.cons(1, Sequence.cons(0, Sequence.cons(2, Sequence.empty)))
        val moved = FiniteSet.empty[Int].insert(0).insert(1)
        (Permutation.tryOf(stored, cycle), Permutation.tryOf(stored, swap)) match
          case (Some(cyclePerm), Some(swapPerm)) =>
            cyclePerm.at(0) == Some(1)
              && cyclePerm.act(moved).exists(_.toList == List(1, 2))
              && cyclePerm.orbit(0).exists(_.equal(stored))
              && swapPerm.orbit(2).exists(_.toList == List(2))
              && swapPerm.orbit(0).exists(_.toList == List(0, 1))
              && cyclePerm.at(9).isEmpty
          case _ => false
      }
    ),
    check(
      "power", {
        val subsets = stored.power
        subsets.toList.map(_.toList) == List(
          Nil,
          List(0),
          List(0, 1),
          List(0, 1, 2),
          List(0, 2),
          List(1),
          List(1, 2),
          List(2)
        )
          && subsets.cardinality == subsets.toList.length
          && FiniteSet.empty[Int].power.toList.map(_.toList) == List(Nil)
      }
    ),
    check(
      "k-subsets", {
        stored.kSubsets(1).toList.map(_.toList) == List(List(0), List(1), List(2))
          && stored.kSubsets(0).toList.map(_.toList) == List(Nil)
          && stored.kSubsets(3).toList.map(_.toList) == List(List(0, 1, 2))
          && stored.kSubsets(4).cardinality == 0
          && stored.kSubsets(1).cardinality == stored.kSubsets(1).toList.length
      }
    ),
    check(
      "permutations", {
        val family = Enumeration.permutations(stored)
        family.toList.map(_.sequence.toList) == List(
          List(0, 1, 2),
          List(0, 2, 1),
          List(1, 0, 2),
          List(1, 2, 0),
          List(2, 0, 1),
          List(2, 1, 0)
        )
          && family.cardinality == family.toList.length
          && Enumeration.permutations(FiniteSet.empty[Int]).cardinality == 1
      }
    ),
    check(
      "set-partitions", {
        val family = Enumeration.setPartitions(stored)
        family.toList.map(_.blocks.toList.map(_.toList)) == List(
          List(List(0), List(1), List(2)),
          List(List(0), List(1, 2)),
          List(List(0, 1), List(2)),
          List(List(0, 1, 2)),
          List(List(0, 2), List(1))
        )
          && family.cardinality == family.toList.length
          && Enumeration.setPartitions(FiniteSet.empty[Int]).cardinality == 1
      }
    ),
    check(
      "functions", {
        val domain = FiniteSet.empty[Int].insert(0).insert(1)
        val codomain = FiniteSet.empty[Int].insert(7).insert(8)
        val family = Enumeration.functions(domain, codomain)
        family.toList.map(function => Function.relation(function).pairs.toList) == List(
          List((0, 7), (1, 7)),
          List((0, 7), (1, 8)),
          List((0, 8), (1, 7)),
          List((0, 8), (1, 8))
        )
          && family.cardinality == family.toList.length
          && Enumeration.functions(FiniteSet.empty[Int], codomain).cardinality == 1
          && Enumeration.functions(domain, FiniteSet.empty[Int]).cardinality == 0
      }
    ),
    check(
      "injections", {
        val domain = FiniteSet.empty[Int].insert(0).insert(1)
        val codomain = FiniteSet.empty[Int].insert(0).insert(1).insert(2)
        val family = Enumeration.injections(domain, codomain)
        val maps = Enumeration.functions(domain, codomain)
        family.toList.map(function => Function.relation(function).pairs.toList) == List(
          List((0, 0), (1, 1)),
          List((0, 0), (1, 2)),
          List((0, 1), (1, 0)),
          List((0, 1), (1, 2)),
          List((0, 2), (1, 0)),
          List((0, 2), (1, 1))
        )
          && family.toList.forall(_.isInjective(domain, codomain))
          && maps.toList.exists(function => !family.contains(function))
          && family.cardinality == family.toList.length
      }
    ),
    check(
      "surjections", {
        val domain = FiniteSet.empty[Int].insert(0).insert(1)
        val codomain = FiniteSet.empty[Int].insert(0).insert(1)
        val family = Enumeration.surjections(domain, codomain)
        family.toList.map(function => Function.relation(function).pairs.toList) == List(
          List((0, 0), (1, 1)),
          List((0, 1), (1, 0))
        )
          && family.toList.forall(_.isSurjective(domain, codomain))
          && Enumeration.functions(domain, codomain).toList.exists(function => !family.contains(function))
          && family.cardinality == family.toList.length
      }
    ),
    check(
      "bijections", {
        val domain = FiniteSet.empty[Int].insert(0).insert(1)
        val family = Enumeration.bijections(domain, domain)
        val larger = domain.insert(2)
        family.toList.map(function => Function.relation(function).pairs.toList) == List(
          List((0, 0), (1, 1)),
          List((0, 1), (1, 0))
        )
          && family.toList.forall(_.isBijective(domain, domain))
          && Enumeration.bijections(domain, larger).cardinality == 0
          && Enumeration.injections(domain, larger).cardinality == Enumeration.injections(domain, larger).toList.length
          && family.cardinality == family.toList.length
      }
    ),
    check(
      "count", {
        val subsets = stored.power
        val perms = Enumeration.permutations(stored)
        Enumeration.count(subsets) == subsets.cardinality
          && Enumeration.count(subsets) == subsets.toList.length
          && Enumeration.count(perms) == perms.toList.length
          && Enumeration.count(perms) == perms.cardinality
      }
    ),
    check(
      "subsets-and-words", {
        val bits = FiniteSet.empty[Int].insert(0).insert(1)
        val subsets = stored.power
        val words = Sequence.words(bits, stored.cardinality)
        Enumeration.count(subsets) == Enumeration.count(words)
          && Enumeration.count(subsets) == subsets.toList.length
          && Enumeration.count(words) == words.toList.length
      }
    ),
    check(
      "complement-counts", {
        val size = stored.cardinality
        val k = 1
        val left = stored.kSubsets(k)
        val right = stored.kSubsets(size - k)
        Enumeration.count(left) == Enumeration.count(right)
          && Enumeration.count(left) == left.toList.length
          && Enumeration.count(right) == right.toList.length
          && Enumeration.count(stored.kSubsets(0)) == Enumeration.count(stored.kSubsets(size))
      }
    ),
    check(
      "subsets-add", {
        val tagged = Enumeration.subsetsBySize(stored)
        val recovered = tagged.map(FiniteSet.snd)
        recovered.equal(stored.power)
          && Enumeration.count(tagged) == Enumeration.count(stored.power)
          && Enumeration.count(tagged) == tagged.toList.length
      }
    ),
    check(
      "bijections-and-permutations", {
        val maps = Enumeration.bijections(stored, stored)
        val perms = Enumeration.permutations(stored)
        Enumeration.count(maps) == Enumeration.count(perms)
          && Enumeration.count(maps) == maps.toList.length
          && Enumeration.count(perms) == perms.toList.length
      }
    ),
    check(
      "longer-word", {
        val alphabet = FiniteSet.empty[Int].insert(0).insert(1)
        val k = 2
        val words = Sequence.words(alphabet, k)
        val shorter = Sequence.words(alphabet, k - 1)
        val pairs = FiniteSet.product(alphabet, shorter)
        Enumeration.count(words) == Enumeration.count(pairs)
          && Enumeration.count(words) == words.toList.length
          && Enumeration.count(pairs) == pairs.toList.length
      }
    ),
    check(
      "function-word", {
        val domain = FiniteSet.empty[Int].insert(0).insert(1)
        val codomain = FiniteSet.empty[Int].insert(5).insert(6)
        val maps = Enumeration.functions(domain, codomain)
        val words = Sequence.words(codomain, domain.cardinality)
        Enumeration.count(maps) == Enumeration.count(words)
          && Enumeration.count(maps) == maps.toList.length
          && Enumeration.count(words) == words.toList.length
      }
    ),
    check(
      "pascal", {
        val element = 0
        val k = 1
        val without = stored.remove(element)
        val omitting = without.kSubsets(k)
        val containing = without.kSubsets(k - 1)
        val tagged = FiniteSet.sum(omitting, containing)
        val whole = stored.kSubsets(k)
        Enumeration.count(whole) == Enumeration.count(tagged)
          && Enumeration.count(whole) == whole.toList.length
          && Enumeration.count(omitting) == omitting.toList.length
          && Enumeration.count(containing) == containing.toList.length
      }
    ),
    check(
      "complement", {
        val forward = Enumeration.complement(stored, 1)
        val backward = Enumeration.complement(stored, stored.cardinality - 1)
        val one = FiniteSet.singleton(0)
        val rest = stored.remove(0)
        forward(one) == Some(rest)
          && backward(rest) == Some(one)
          && forward.isBijective(stored.kSubsets(1), stored.kSubsets(stored.cardinality - 1))
          && Enumeration.count(stored.kSubsets(1)) == Enumeration.count(stored.kSubsets(stored.cardinality - 1))
      }
    ),
    check(
      "subset-word", {
        val bits = FiniteSet.empty[Int].insert(0).insert(1)
        val chosen = FiniteSet.empty[Int].insert(0).insert(2)
        val word = Sequence.cons(1, Sequence.cons(0, Sequence.cons(1, Sequence.empty)))
        val forward = Enumeration.subsetToWord(stored)
        forward(chosen) == Some(word)
          && Enumeration.wordToSubset(stored, word).equal(chosen)
          && forward.isBijective(stored.power, Sequence.words(bits, stored.cardinality))
      }
    ),
    check(
      "permutation-bijection", {
        val identity = Permutation.tryOf(
          stored,
          Sequence.cons(0, Sequence.cons(1, Sequence.cons(2, Sequence.empty)))
        ).get
        val map = Enumeration.permutationToMap(identity)
        val back = Enumeration.mapToPermutation(stored, map)
        val family = Enumeration.permutationsToBijections(stored)
        back == Some(identity)
          && map(0) == Some(0)
          && map(2) == Some(2)
          && family.isBijective(Enumeration.permutations(stored), Enumeration.bijections(stored, stored))
      }
    ),
    check(
      "pascal-split", {
        val element = 0
        val k = 1
        val without = stored.remove(element)
        val split = Enumeration.pascalSplit(stored, element, k)
        val tagged = FiniteSet.sum(without.kSubsets(k), without.kSubsets(k - 1))
        split(FiniteSet.singleton(0)) == Some(FiniteSet.injectRight(FiniteSet.empty[Int]))
          && split(FiniteSet.singleton(1)) == Some(FiniteSet.injectLeft(FiniteSet.singleton(1)))
          && split.isBijective(stored.kSubsets(k), tagged)
      }
    ),
    check(
      "word-split", {
        val alphabet = FiniteSet.empty[Int].insert(0).insert(1)
        val k = 2
        val word = Sequence.cons(0, Sequence.cons(1, Sequence.empty))
        val prefix = Sequence.cons(0, Sequence.empty)
        val split = Enumeration.splitWord(alphabet, k)
        val product = FiniteSet.product(alphabet, Sequence.words(alphabet, k - 1))
        split(word) == Some((1, prefix))
          && Enumeration.joinWord(1, prefix) == word
          && split.isBijective(Sequence.words(alphabet, k), product)
      }
    ),
    check(
      "graph", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val edges = FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)
        val path = Graph.tryOf(stored, edges)
        val tooBig = Graph.tryOf(stored, FiniteSet.singleton(stored))
        val outside = Graph.tryOf(stored, FiniteSet.singleton(FiniteSet.empty[Int].insert(0).insert(3)))
        path.isDefined
          && path.get.vertices.equal(stored)
          && path.get.edges.equal(edges)
          && tooBig.isEmpty
          && outside.isEmpty
      }
    ),
    check(
      "neighbors", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        path.neighbors(1).get.equal(FiniteSet.empty[Int].insert(0).insert(2))
          && path.neighbors(0).get.equal(FiniteSet.singleton(1))
          && path.neighbors(3).isEmpty
      }
    ),
    check(
      "degree", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        val atMiddle = path.edges.filter(edge => edge.contains(1))
        path.degree(1) == Some(Enumeration.count(atMiddle))
          && path.degree(0) == Some(1)
          && Enumeration.count(atMiddle) == atMiddle.toList.length
          && path.degree(3).isEmpty
      }
    ),
    check(
      "walk", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        val along = Sequence.cons(0, Sequence.cons(1, Sequence.cons(2, Sequence.empty)))
        val jump = Sequence.cons(0, Sequence.cons(2, Sequence.empty))
        path.tryWalk(along) == Some(along)
          && path.tryWalk(jump).isEmpty
          && path.tryWalk(Sequence.cons(0, Sequence.empty)) == Some(Sequence.cons(0, Sequence.empty))
          && path.tryWalk(Sequence.empty).isEmpty
      }
    ),
    check(
      "reachable", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        val alone = Graph.tryOf(stored, FiniteSet.singleton(edge)).get
        path.reachable(0).get.equal(stored)
          && alone.reachable(0).get.equal(FiniteSet.empty[Int].insert(0).insert(1))
          && !alone.reachable(0).get.contains(2)
          && path.reachable(3).isEmpty
      }
    ),
    check(
      "connected", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        val leftOut = Graph.tryOf(stored, FiniteSet.singleton(edge)).get
        path.isConnected && !leftOut.isConnected
      }
    ),
    check(
      "tree", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val closing = FiniteSet.empty[Int].insert(0).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        val cycle = Graph.tryOf(stored, path.edges.insert(closing)).get
        path.isTree
          && !cycle.isTree
          && Enumeration.count(path.edges) + 1 == Enumeration.count(path.vertices)
          && Enumeration.count(cycle.edges) + 1 != Enumeration.count(cycle.vertices)
      }
    )
  )

  if !results.forall(identity) then sys.exit(1)
