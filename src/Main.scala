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

  def edge(left: Int, right: Int): FiniteSet.Set[Int] =
    FiniteSet.empty[Int].insert(left).insert(right)

  def graphOn(vertices: FiniteSet.Set[Int], edges: FiniteSet.Set[Int]*): Graph.Graph[Int] =
    Graph.tryOf(vertices, edges.foldLeft(FiniteSet.empty[FiniteSet.Set[Int]])(_.insert(_))).get

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
    ),
    check(
      "prufer", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        val word = Sequence.cons(1, Sequence.empty)
        val decoded = Graph.pruferDecode(stored, word)
        val trees = Graph.labeledTrees(stored)
        val words = Sequence.words(stored, stored.cardinality - 2)
        val one = Graph.tryOf(FiniteSet.singleton(0), FiniteSet.empty).get
        Graph.pruferEncode(path) == Some(word)
          && decoded.get.edges.equal(path.edges)
          && Enumeration.count(trees) == Enumeration.count(words)
          && Enumeration.count(trees) == trees.toList.length
          && Enumeration.count(words) == words.toList.length
          && Graph.pruferEncode(one) == Some(Sequence.empty)
          && Graph.pruferEncode(Graph.tryOf(stored, path.edges.insert(FiniteSet.empty[Int].insert(0).insert(2))).get).isEmpty
      }
    ),
    check(
      "relabel", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        val reversal = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((0, 2)).insert((1, 1)).insert((2, 0))
          )
        ).get
        val inverse = Function.tryOfRelation(
          Relation.ofPairs(
            Relation.pairs(Function.relation(reversal)).toList.foldLeft(FiniteSet.empty[(Int, Int)]) {
              (acc, pair) => acc.insert((pair._2, pair._1))
            }
          )
        ).get
        val moved = Graph.relabel(path, reversal)
        val back = moved.flatMap(graph => Graph.relabel(graph, inverse))
        moved.isDefined && back.get.vertices.equal(path.vertices) && back.get.edges.equal(path.edges)
      }
    ),
    check(
      "isomorphism", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val closing = FiniteSet.empty[Int].insert(0).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        val triangle = Graph.tryOf(stored, path.edges.insert(closing)).get
        val reversal = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((0, 2)).insert((1, 1)).insert((2, 0))
          )
        ).get
        Graph.isIsomorphism(reversal, path, path) && !Graph.isIsomorphism(reversal, path, triangle)
      }
    ),
    check(
      "isomorphisms", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        val reversal = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((0, 2)).insert((1, 1)).insert((2, 0))
          )
        ).get
        val swap = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((0, 1)).insert((1, 0)).insert((2, 2))
          )
        ).get
        val family = Graph.isomorphisms(path, path)
        family.contains(Function.identity(stored))
          && family.contains(reversal)
          && !family.contains(swap)
          && Enumeration.count(family) == family.toList.length
      }
    ),
    check(
      "path-star", {
        val four = stored.insert(3)
        def edge(left: Int, right: Int): FiniteSet.Set[Int] =
          FiniteSet.empty[Int].insert(left).insert(right)
        def graph(edges: FiniteSet.Set[Int]*): Graph.Graph[Int] =
          Graph.tryOf(four, edges.foldLeft(FiniteSet.empty[FiniteSet.Set[Int]])(_.insert(_))).get
        val path = graph(edge(0, 1), edge(1, 2), edge(2, 3))
        val star = graph(edge(0, 1), edge(0, 2), edge(0, 3))
        val family = Graph.isomorphisms(path, star)
        path.isTree && star.isTree && family.toList.isEmpty && Enumeration.count(family) == family.toList.length
      }
    ),
    check(
      "automorphisms", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val closing = FiniteSet.empty[Int].insert(0).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        val triangle = Graph.tryOf(stored, path.edges.insert(closing)).get
        val reversal = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((0, 2)).insert((1, 1)).insert((2, 0))
          )
        ).get
        val autos = Graph.automorphisms(path)
        autos.contains(Function.identity(stored))
          && autos.contains(reversal)
          && Graph.automorphisms(triangle).equal(Enumeration.bijections(stored, stored))
      }
    ),
    check(
      "vertex-orbit", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        Graph.vertexOrbit(path, 1).get.equal(FiniteSet.singleton(1))
          && Graph.vertexOrbit(path, 0).get.equal(FiniteSet.empty[Int].insert(0).insert(2))
          && Graph.vertexOrbit(path, 3).isEmpty
      }
    ),
    check(
      "edge-orbit", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        val alone = Graph.tryOf(stored, FiniteSet.singleton(edge)).get
        Graph.edgeOrbit(path, edge).get.equal(path.edges)
          && Graph.edgeOrbit(alone, edge).get.equal(FiniteSet.singleton(edge))
          && Graph.edgeOrbit(path, FiniteSet.empty[Int].insert(0).insert(2)).isEmpty
      }
    ),
    check(
      "relabelings", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val alone = Graph.tryOf(stored, FiniteSet.singleton(edge)).get
        val family = Graph.relabelings(alone)
        val edgeSets = family.toList.foldLeft(FiniteSet.empty[FiniteSet.Set[FiniteSet.Set[Int]]]) { (acc, graph) =>
          acc.insert(graph.edges)
        }
        val expected = stored.kSubsets(2).map(edge => FiniteSet.singleton(edge))
        edgeSets.equal(expected) && Enumeration.count(family) == family.toList.length
      }
    ),
    check(
      "canonical", {
        val empty = Graph.tryOf(stored, FiniteSet.empty).get
        val triangle = Graph.tryOf(stored, stored.kSubsets(2)).get
        val oneEdges = stored.kSubsets(2).toList.map { edge =>
          Graph.tryOf(stored, FiniteSet.singleton(edge)).get
        }
        val least = oneEdges.map(Graph.canonical).min
        Graph.canonical(empty).edges.equal(empty.edges)
          && Graph.canonical(triangle).edges.equal(triangle.edges)
          && oneEdges.forall(graph => Graph.canonical(graph).edges.equal(least.edges))
      }
    ),
    check(
      "canonical-stable", {
        val edge = FiniteSet.empty[Int].insert(1).insert(2)
        val graph = Graph.tryOf(stored, FiniteSet.singleton(edge)).get
        val move = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((1, 0)).insert((2, 1)).insert((0, 2))
          )
        ).get
        val image = Graph.relabel(graph, move).get
        val canonical = Graph.canonical(graph)
        !graph.edges.equal(canonical.edges)
          && Graph.canonical(image).edges.equal(canonical.edges)
          && Graph.canonical(canonical).edges.equal(canonical.edges)
      }
    ),
    check(
      "same-unlabeled", {
        val four = stored.insert(3)
        def edge(left: Int, right: Int): FiniteSet.Set[Int] =
          FiniteSet.empty[Int].insert(left).insert(right)
        def graph(edges: FiniteSet.Set[Int]*): Graph.Graph[Int] =
          Graph.tryOf(four, edges.foldLeft(FiniteSet.empty[FiniteSet.Set[Int]])(_.insert(_))).get
        val path = graph(edge(0, 1), edge(1, 2), edge(2, 3))
        val star = graph(edge(0, 1), edge(0, 2), edge(0, 3))
        val cycle = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet
              .empty[(Int, Int)]
              .insert((0, 1))
              .insert((1, 2))
              .insert((2, 3))
              .insert((3, 0))
          )
        ).get
        val moved = Graph.relabel(path, cycle).get
        Graph.sameUnlabeled(path, moved)
          && Graph.isomorphisms(path, moved).toList.nonEmpty
          && !Graph.sameUnlabeled(path, star)
          && Graph.isomorphisms(path, star).toList.isEmpty
      }
    ),
    check(
      "all-graphs", {
        val graphs = Graph.allGraphs(stored)
        val edgeSets = graphs.toList.foldLeft(FiniteSet.empty[FiniteSet.Set[FiniteSet.Set[Int]]]) { (acc, graph) =>
          acc.insert(graph.edges)
        }
        val potential = stored.kSubsets(2).power
        val path = Graph.tryOf(
          stored,
          FiniteSet.empty[FiniteSet.Set[Int]].insert(FiniteSet.empty[Int].insert(0).insert(1)).insert(
            FiniteSet.empty[Int].insert(1).insert(2)
          )
        ).get
        edgeSets.equal(potential)
          && graphs.contains(Graph.tryOf(stored, FiniteSet.empty).get)
          && graphs.contains(path)
          && Enumeration.count(graphs) == Enumeration.count(potential)
          && Enumeration.count(graphs) == graphs.toList.length
      }
    ),
    check(
      "edge-graph", {
        val forward = Graph.edgeGraph(stored)
        val emptyEdges = FiniteSet.empty[FiniteSet.Set[Int]]
        val pathEdges = FiniteSet
          .empty[FiniteSet.Set[Int]]
          .insert(FiniteSet.empty[Int].insert(0).insert(1))
          .insert(FiniteSet.empty[Int].insert(1).insert(2))
        forward(emptyEdges).get.edges.equal(emptyEdges)
          && forward(pathEdges).get.edges.equal(pathEdges)
          && forward.isBijective(stored.kSubsets(2).power, Graph.allGraphs(stored))
      }
    ),
    check(
      "census", {
        val empty = Graph.tryOf(stored, FiniteSet.empty).get
        val one = Graph.tryOf(stored, FiniteSet.singleton(FiniteSet.empty[Int].insert(0).insert(1))).get
        val path = Graph.tryOf(
          stored,
          FiniteSet
            .empty[FiniteSet.Set[Int]]
            .insert(FiniteSet.empty[Int].insert(0).insert(1))
            .insert(FiniteSet.empty[Int].insert(1).insert(2))
        ).get
        val triangle = Graph.tryOf(stored, stored.kSubsets(2)).get
        val listed = List(empty, one, path, triangle).foldLeft(FiniteSet.empty[Graph.Graph[Int]]) { (acc, graph) =>
          acc.insert(Graph.canonical(graph))
        }
        val census = Graph.census(stored)
        census.equal(listed)
          && Graph.allGraphs(stored).toList.forall(graph => census.contains(Graph.canonical(graph)))
          && Enumeration.count(census) == census.toList.length
      }
    ),
    check(
      "orbit-split", {
        val graph = Graph.tryOf(stored, FiniteSet.singleton(FiniteSet.empty[Int].insert(1).insert(2))).get
        val orbit = Graph.relabelings(graph)
        val section = Graph.orbitSection(graph)
        val split = Graph.orbitSplit(graph)
        orbit.toList.forall { image =>
          section(image).exists(mapping => Graph.isIsomorphism(mapping, graph, image))
        }
          && split.isBijective(
            Enumeration.bijections(stored, stored),
            FiniteSet.product(orbit, Graph.automorphisms(graph))
          )
      }
    ),
    check(
      "connected-census", {
        val empty = Graph.canonical(Graph.tryOf(stored, FiniteSet.empty).get)
        val one = Graph.canonical(
          Graph.tryOf(stored, FiniteSet.singleton(FiniteSet.empty[Int].insert(0).insert(1))).get
        )
        val path = Graph.canonical(
          Graph.tryOf(
            stored,
            FiniteSet
              .empty[FiniteSet.Set[Int]]
              .insert(FiniteSet.empty[Int].insert(0).insert(1))
              .insert(FiniteSet.empty[Int].insert(1).insert(2))
          ).get
        )
        val triangle = Graph.canonical(Graph.tryOf(stored, stored.kSubsets(2)).get)
        val connected = Graph.connectedCensus(stored)
        val listed = FiniteSet.empty[Graph.Graph[Int]].insert(path).insert(triangle)
        connected.equal(listed)
          && !connected.contains(empty)
          && !connected.contains(one)
          && connected.toList.forall(_.isConnected)
          && Enumeration.count(connected) == connected.toList.length
      }
    ),
    check(
      "four-graphs", {
        val four = stored.insert(3)
        def edge(left: Int, right: Int): FiniteSet.Set[Int] =
          FiniteSet.empty[Int].insert(left).insert(right)
        def graph(edges: FiniteSet.Set[Int]*): Graph.Graph[Int] =
          Graph.tryOf(four, edges.foldLeft(FiniteSet.empty[FiniteSet.Set[Int]])(_.insert(_))).get
        val path = graph(edge(0, 1), edge(1, 2), edge(2, 3))
        val star = graph(edge(0, 1), edge(0, 2), edge(0, 3))
        val graphs = Graph.allGraphs(four)
        val potential = four.kSubsets(2).power
        val forward = Graph.edgeGraph(four)
        Enumeration.count(graphs) == Enumeration.count(potential)
          && Enumeration.count(graphs) == graphs.toList.length
          && forward.isBijective(potential, graphs)
          && forward(path.edges).get.edges.equal(path.edges)
          && forward(star.edges).get.edges.equal(star.edges)
      }
    ),
    check(
      "four-classes", {
        val four = stored.insert(3)
        def edge(left: Int, right: Int): FiniteSet.Set[Int] =
          FiniteSet.empty[Int].insert(left).insert(right)
        def graph(edges: FiniteSet.Set[Int]*): Graph.Graph[Int] =
          Graph.tryOf(four, edges.foldLeft(FiniteSet.empty[FiniteSet.Set[Int]])(_.insert(_))).get
        val path = graph(edge(0, 1), edge(1, 2), edge(2, 3))
        val star = graph(edge(0, 1), edge(0, 2), edge(0, 3))
        val cycle = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((0, 1)).insert((1, 2)).insert((2, 3)).insert((3, 0))
          )
        ).get
        val moved = Graph.relabel(path, cycle).get
        val census = Graph.census(four)
        val pathCanonical = Graph.canonical(path)
        val starCanonical = Graph.canonical(star)
        census.contains(pathCanonical)
          && census.contains(starCanonical)
          && !pathCanonical.edges.equal(starCanonical.edges)
          && Graph.canonical(moved).edges.equal(pathCanonical.edges)
      }
    ),
    check(
      "tree-census", {
        val four = stored.insert(3)
        def edge(left: Int, right: Int): FiniteSet.Set[Int] =
          FiniteSet.empty[Int].insert(left).insert(right)
        def graph(edges: FiniteSet.Set[Int]*): Graph.Graph[Int] =
          Graph.tryOf(four, edges.foldLeft(FiniteSet.empty[FiniteSet.Set[Int]])(_.insert(_))).get
        val path = Graph.canonical(graph(edge(0, 1), edge(1, 2), edge(2, 3)))
        val star = Graph.canonical(graph(edge(0, 1), edge(0, 2), edge(0, 3)))
        val trees = Graph.treeCensus(four)
        val forms = Graph.canonicalTrees(four)
        trees.equal(forms)
          && trees.contains(path)
          && trees.contains(star)
          && !path.edges.equal(star.edges)
          && Enumeration.count(trees) == trees.toList.length
      }
    ),
    check(
      "degree-sequence", {
        val four = stored.insert(3)
        def edge(left: Int, right: Int): FiniteSet.Set[Int] =
          FiniteSet.empty[Int].insert(left).insert(right)
        def graph(edges: FiniteSet.Set[Int]*): Graph.Graph[Int] =
          Graph.tryOf(four, edges.foldLeft(FiniteSet.empty[FiniteSet.Set[Int]])(_.insert(_))).get
        val path = graph(edge(0, 1), edge(1, 2), edge(2, 3))
        val star = graph(edge(0, 1), edge(0, 2), edge(0, 3))
        val pathDegrees = Graph.degreeSequence(path)
        val starDegrees = Graph.degreeSequence(star)
        pathDegrees.toList == path.vertices.toList.map(vertex => path.degree(vertex).get)
          && starDegrees.toList == star.vertices.toList.map(vertex => star.degree(vertex).get)
          && pathDegrees != starDegrees
      }
    ),
    check(
      "degree-invariant", {
        val graph = Graph.tryOf(stored, FiniteSet.singleton(FiniteSet.empty[Int].insert(1).insert(2))).get
        val move = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((1, 0)).insert((2, 1)).insert((0, 2))
          )
        ).get
        val image = Graph.relabel(graph, move).get
        Graph.degreeSequence(graph) != Graph.degreeSequence(image)
          && Graph.sortedDegreeSequence(graph) == Graph.sortedDegreeSequence(image)
      }
    ),
    check(
      "flags", {
        val edge = FiniteSet.empty[Int].insert(0).insert(1)
        val other = FiniteSet.empty[Int].insert(1).insert(2)
        val path = Graph.tryOf(stored, FiniteSet.empty[FiniteSet.Set[Int]].insert(edge).insert(other)).get
        val flags = Graph.flags(path)
        val copies = FiniteSet.sum(path.edges, path.edges)
        Enumeration.count(flags) == Graph.degreeSequence(path).toList.sum
          && Enumeration.count(flags) == Enumeration.count(copies)
          && Enumeration.count(flags) == flags.toList.length
          && path.edges.toList.forall { edge =>
            Enumeration.count(flags.filter((_, incident) => incident.equal(edge))) == edge.cardinality
          }
      }
    ),
    check(
      "degree-census", {
        val four = stored.insert(3)
        val recorded = Graph.allGraphs(four).toList.map { graph =>
          (Graph.canonical(graph), Graph.sortedDegreeSequence(graph))
        }
        recorded.forall { (leftCanonical, leftDegrees) =>
          recorded.forall { (rightCanonical, rightDegrees) =>
            (leftDegrees == rightDegrees) == leftCanonical.edges.equal(rightCanonical.edges)
          }
        }
      }
    ),
    check(
      "degree-pair", {
        val five = stored.insert(3).insert(4)
        def edge(left: Int, right: Int): FiniteSet.Set[Int] =
          FiniteSet.empty[Int].insert(left).insert(right)
        def graph(edges: FiniteSet.Set[Int]*): Graph.Graph[Int] =
          Graph.tryOf(five, edges.foldLeft(FiniteSet.empty[FiniteSet.Set[Int]])(_.insert(_))).get
        val path = graph(edge(0, 1), edge(1, 2), edge(2, 3), edge(3, 4))
        val split = graph(edge(0, 1), edge(1, 2), edge(2, 0), edge(3, 4))
        val family = Graph.isomorphisms(path, split)
        Graph.sortedDegreeSequence(path) == Graph.sortedDegreeSequence(split)
          && !Graph.sameUnlabeled(path, split)
          && family.toList.isEmpty
          && Enumeration.count(family) == family.toList.length
      }
    ),
    check(
      "graph-complement", {
        val empty = Graph.tryOf(stored, FiniteSet.empty).get
        val triangle = Graph.tryOf(stored, stored.kSubsets(2)).get
        val path = Graph.tryOf(
          stored,
          FiniteSet
            .empty[FiniteSet.Set[Int]]
            .insert(FiniteSet.empty[Int].insert(0).insert(1))
            .insert(FiniteSet.empty[Int].insert(1).insert(2))
        ).get
        Graph.complement(empty).edges.equal(triangle.edges)
          && Graph.complement(triangle).edges.equal(empty.edges)
          && Graph.complement(Graph.complement(path)).edges.equal(path.edges)
      }
    ),
    check(
      "complement-relabel", {
        val graph = Graph.tryOf(stored, FiniteSet.singleton(FiniteSet.empty[Int].insert(1).insert(2))).get
        val move = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((1, 0)).insert((2, 1)).insert((0, 2))
          )
        ).get
        val carried = Graph.relabel(Graph.complement(graph), move).get
        Graph.complement(Graph.relabel(graph, move).get).edges.equal(carried.edges)
      }
    ),
    check(
      "complement-census", {
        val census = Graph.census(stored)
        val images = census.toList.foldLeft(FiniteSet.empty[Graph.Graph[Int]]) { (acc, graph) =>
          acc.insert(Graph.canonical(Graph.complement(graph)))
        }
        val empty = Graph.canonical(Graph.tryOf(stored, FiniteSet.empty).get)
        val one = Graph.canonical(
          Graph.tryOf(stored, FiniteSet.singleton(FiniteSet.empty[Int].insert(0).insert(1))).get
        )
        val path = Graph.canonical(
          Graph.tryOf(
            stored,
            FiniteSet
              .empty[FiniteSet.Set[Int]]
              .insert(FiniteSet.empty[Int].insert(0).insert(1))
              .insert(FiniteSet.empty[Int].insert(1).insert(2))
          ).get
        )
        val triangle = Graph.canonical(Graph.tryOf(stored, stored.kSubsets(2)).get)
        images.equal(census)
          && Graph.canonical(Graph.complement(empty)).edges.equal(triangle.edges)
          && Graph.canonical(Graph.complement(triangle)).edges.equal(empty.edges)
          && Graph.canonical(Graph.complement(one)).edges.equal(path.edges)
          && Graph.canonical(Graph.complement(path)).edges.equal(one.edges)
      }
    ),
    check(
      "complement-pair", {
        val five = stored.insert(3).insert(4)
        def edge(left: Int, right: Int): FiniteSet.Set[Int] =
          FiniteSet.empty[Int].insert(left).insert(right)
        def graph(edges: FiniteSet.Set[Int]*): Graph.Graph[Int] =
          Graph.tryOf(five, edges.foldLeft(FiniteSet.empty[FiniteSet.Set[Int]])(_.insert(_))).get
        val path = Graph.complement(graph(edge(0, 1), edge(1, 2), edge(2, 3), edge(3, 4)))
        val split = Graph.complement(graph(edge(0, 1), edge(1, 2), edge(2, 0), edge(3, 4)))
        path.isConnected
          && split.isConnected
          && Graph.sortedDegreeSequence(path) == Graph.sortedDegreeSequence(split)
          && !Graph.sameUnlabeled(path, split)
      }
    ),
    check(
      "onto-canonical", {
        val graph = Graph.tryOf(stored, FiniteSet.singleton(FiniteSet.empty[Int].insert(1).insert(2))).get
        val autos = Graph.automorphisms(graph)
        val onto = Graph.ontoCanonical(graph)
        val target = Graph.isomorphisms(graph, Graph.canonical(graph))
        val chosen = onto(Function.identity(stored)).get
        val back = Function.tryOfRelation(
          Relation.ofPairs(
            Relation.pairs(Function.relation(chosen)).toList.foldLeft(FiniteSet.empty[(Int, Int)]) { (acc, pair) =>
              acc.insert((pair._2, pair._1))
            }
          )
        ).get
        onto.isBijective(autos, target)
          && autos.toList.forall { automorphism =>
            back.compose(onto(automorphism).get).contains(automorphism)
          }
      }
    ),
    check(
      "induced", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        val inside = Graph.induced(path, stored).get
        val apart = Graph.induced(path, edge(0, 2)).get
        inside.vertices.equal(stored)
          && inside.edges.equal(graphOn(stored, edge(0, 1), edge(1, 2)).edges)
          && apart.vertices.equal(edge(0, 2))
          && Enumeration.count(apart.edges) == 0
          && Graph.induced(path, four).get.edges.equal(path.edges)
          && Graph.induced(path, stored.insert(9)).isEmpty
      }
    ),
    check(
      "induced-relabel", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        val cycle = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((0, 1)).insert((1, 2)).insert((2, 3)).insert((3, 0))
          )
        ).get
        val moved = Graph.relabel(path, cycle).get
        four.power.toList.forall { chosen =>
          val carried = Graph.relabel(Graph.induced(path, chosen).get, cycle).get
          val cut = Graph.induced(moved, chosen.map(vertex => cycle(vertex).get)).get
          carried.vertices.equal(cut.vertices) && carried.edges.equal(cut.edges)
        }
      }
    ),
    check(
      "induced-complement", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        four.power.toList.forall { chosen =>
          val inside = Graph.complement(Graph.induced(path, chosen).get)
          val outside = Graph.induced(Graph.complement(path), chosen).get
          inside.vertices.equal(outside.vertices) && inside.edges.equal(outside.edges)
        }
      }
    ),
    check(
      "component", {
        val five = stored.insert(3).insert(4)
        val split = graphOn(five, edge(0, 1), edge(1, 2), edge(2, 0), edge(3, 4))
        val triangle = Graph.component(split, 0).get
        val pair = Graph.component(split, 3).get
        triangle.vertices.equal(stored)
          && triangle.edges.equal(stored.kSubsets(2))
          && pair.vertices.equal(edge(3, 4))
          && pair.edges.equal(FiniteSet.singleton(edge(3, 4)))
          && Graph.component(split, 1).get.edges.equal(triangle.edges)
          && triangle.isConnected
          && pair.isConnected
          && Graph.component(split, 9).isEmpty
      }
    ),
    check(
      "components", {
        val five = stored.insert(3).insert(4)
        val split = graphOn(five, edge(0, 1), edge(1, 2), edge(2, 0), edge(3, 4))
        val path = graphOn(five, edge(0, 1), edge(1, 2), edge(2, 3), edge(3, 4))
        val blocks = Graph.components(split).blocks
        val listed = FiniteSet.empty[FiniteSet.Set[Int]].insert(stored).insert(edge(3, 4))
        blocks.equal(listed)
          && Graph.components(split).ambient.equal(five)
          && Graph.components(path).blocks.equal(FiniteSet.singleton(five))
          && Enumeration.count(Graph.components(graphOn(stored)).blocks) == Enumeration.count(stored)
          && Enumeration.count(Graph.components(graphOn(FiniteSet.empty[Int])).blocks) == 0
      }
    ),
    check(
      "component-edges", {
        val four = stored.insert(3)
        Graph.allGraphs(four).toList.forall { graph =>
          val blocks = Graph.components(graph).blocks
          val pieces = blocks.toList.map(block => Graph.induced(graph, block).get.edges)
          val gathered = pieces.foldLeft(FiniteSet.empty[FiniteSet.Set[Int]])((acc, piece) => acc.union(piece))
          gathered.equal(graph.edges)
            && pieces.map(piece => Enumeration.count(piece)).sum == Enumeration.count(graph.edges)
            && graph.isConnected == (Enumeration.count(blocks) == 1)
        }
      }
    ),
    check(
      "forest", {
        val four = stored.insert(3)
        val five = four.insert(4)
        val split = graphOn(five, edge(0, 1), edge(1, 2), edge(2, 0), edge(3, 4))
        val pairs = graphOn(four, edge(0, 1), edge(2, 3))
        pairs.isForest
          && !pairs.isTree
          && !split.isForest
          && graphOn(FiniteSet.empty[Int]).isForest
          && Graph.allGraphs(four).toList.forall { graph =>
            val parts = Graph.components(graph).blocks.toList.map(block => Graph.induced(graph, block).get)
            graph.isForest == parts.forall(_.isTree)
              && graph.isTree == (graph.isForest && graph.isConnected)
          }
      }
    ),
    check(
      "deck", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        val end = Graph.card(path, 0).get
        val middle = Graph.card(path, 1).get
        val deck = Graph.deck(path)
        val shortPath = Graph.canonical(graphOn(stored, edge(0, 1), edge(1, 2)))
        val one = Graph.canonical(graphOn(stored, edge(0, 1)))
        val cards = deck.support
        end.vertices.equal(four.remove(0))
          && end.edges.equal(FiniteSet.empty[FiniteSet.Set[Int]].insert(edge(1, 2)).insert(edge(2, 3)))
          && middle.edges.equal(FiniteSet.singleton(edge(2, 3)))
          && Graph.card(path, 9).isEmpty
          && deck.multiplicity(shortPath) == 2
          && deck.multiplicity(one) == 2
          && Enumeration.count(cards) == 2
          && cards.toList.map(card => deck.multiplicity(card)).sum == Enumeration.count(four)
      }
    ),
    check(
      "deck-relabel", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        val star = graphOn(four, edge(0, 1), edge(0, 2), edge(0, 3))
        val cycle = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((0, 1)).insert((1, 2)).insert((2, 3)).insert((3, 0))
          )
        ).get
        Graph.deck(Graph.relabel(path, cycle).get) == Graph.deck(path)
          && Graph.deck(Graph.relabel(star, cycle).get) == Graph.deck(star)
          && Graph.deck(path) != Graph.deck(star)
      }
    ),
    check(
      "deck-census", {
        val four = stored.insert(3)
        val recorded = Graph.allGraphs(four).toList.map { graph =>
          (Graph.canonical(graph), Graph.deck(graph))
        }
        recorded.forall { (leftCanonical, leftDeck) =>
          recorded.forall { (rightCanonical, rightDeck) =>
            (leftDeck == rightDeck) == leftCanonical.edges.equal(rightCanonical.edges)
          }
        }
      }
    ),
    check(
      "deck-pair", {
        val two = edge(0, 1)
        val joined = graphOn(two, edge(0, 1))
        val apart = graphOn(two)
        Graph.deck(joined) == Graph.deck(apart)
          && !Graph.sameUnlabeled(joined, apart)
          && Enumeration.count(Graph.deck(joined).support) == 1
      }
    ),
    check(
      "self-complementary", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        val star = graphOn(four, edge(0, 1), edge(0, 2), edge(0, 3))
        Graph.isSelfComplementary(path) && !Graph.isSelfComplementary(star)
      }
    ),
    check(
      "self-small", {
        val one = graphOn(FiniteSet.singleton(0))
        val two = FiniteSet.empty[Int].insert(0).insert(1)
        val empty = graphOn(stored)
        val oneEdge = graphOn(stored, edge(0, 1))
        val path = graphOn(stored, edge(0, 1), edge(1, 2))
        val triangle = graphOn(stored, edge(0, 1), edge(1, 2), edge(0, 2))
        Graph.isSelfComplementary(one)
          && !Graph.isSelfComplementary(graphOn(two, edge(0, 1)))
          && !Graph.isSelfComplementary(graphOn(two))
          && !Graph.isSelfComplementary(empty)
          && !Graph.isSelfComplementary(oneEdge)
          && !Graph.isSelfComplementary(path)
          && !Graph.isSelfComplementary(triangle)
      }
    ),
    check(
      "self-relabel", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        val star = graphOn(four, edge(0, 1), edge(0, 2), edge(0, 3))
        val cycle = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((0, 1)).insert((1, 2)).insert((2, 3)).insert((3, 0))
          )
        ).get
        val movedPath = Graph.relabel(path, cycle).get
        val movedStar = Graph.relabel(star, cycle).get
        Graph.isSelfComplementary(movedPath)
          && !Graph.isSelfComplementary(movedStar)
          && Graph.sameUnlabeled(movedPath, path)
      }
    ),
    check(
      "self-family", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        val star = graphOn(four, edge(0, 1), edge(0, 2), edge(0, 3))
        val family = Graph.selfComplementary(four)
        family.contains(path)
          && !family.contains(star)
          && Enumeration.count(family) == family.toList.length
      }
    ),
    check(
      "self-census", {
        val four = stored.insert(3)
        val path = Graph.canonical(graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3)))
        val census = Graph.selfComplementaryCensus(four)
        census.equal(FiniteSet.singleton(path))
          && Graph.selfComplementary(four).toList.forall { graph =>
            Graph.canonical(graph).edges.equal(path.edges)
          }
      }
    ),
    check(
      "self-census-small", {
        val one = FiniteSet.singleton(0)
        val two = FiniteSet.empty[Int].insert(0).insert(1)
        val alone = Graph.selfComplementaryCensus(one)
        val pair = Graph.selfComplementaryCensus(two)
        val triple = Graph.selfComplementaryCensus(stored)
        alone.equal(FiniteSet.singleton(graphOn(one)))
          && pair.toList.isEmpty
          && triple.toList.isEmpty
          && Enumeration.count(alone) == alone.toList.length
          && Enumeration.count(pair) == pair.toList.length
          && Enumeration.count(triple) == triple.toList.length
      }
    ),
    check(
      "complementing-map", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        val phi = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((0, 2)).insert((1, 0)).insert((2, 3)).insert((3, 1))
          )
        ).get
        val inverse = Function.tryOfRelation(
          Relation.ofPairs(
            Relation.pairs(Function.relation(phi)).toList.foldLeft(FiniteSet.empty[(Int, Int)]) { (acc, pair) =>
              acc.insert((pair._2, pair._1))
            }
          )
        ).get
        val carried = Graph.relabel(path, phi).get
        val back = Graph.relabel(Graph.complement(path), inverse).get
        carried.edges.equal(Graph.complement(path).edges) && back.edges.equal(path.edges)
      }
    ),
    check(
      "complementing", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        val star = graphOn(four, edge(0, 1), edge(0, 2), edge(0, 3))
        val phi = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((0, 2)).insert((1, 0)).insert((2, 3)).insert((3, 1))
          )
        ).get
        val family = Graph.complementing(path)
        val none = Graph.complementing(star)
        family.contains(phi)
          && Enumeration.count(family) == family.toList.length
          && none.toList.isEmpty
          && Enumeration.count(none) == none.toList.length
      }
    ),
    check(
      "complementing-square", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        val autos = Graph.automorphisms(path)
        val identity = Function.identity(four)
        Graph.complementing(path).toList.forall { phi =>
          val squared = phi.compose(phi).get
          autos.contains(squared) && squared.compose(squared).contains(identity)
        }
      }
    ),
    check(
      "edge-halves", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        val star = graphOn(four, edge(0, 1), edge(0, 2), edge(0, 3))
        def covered(graph: Graph.Graph[Int]): Boolean =
          val halves = Graph.edgeHalves(graph)
          val tagged = FiniteSet.sum(graph.edges, Graph.complement(graph).edges)
          halves(FiniteSet.injectLeft(graph.edges.toList.head)).contains(graph.edges.toList.head)
            && halves.isBijective(tagged, four.kSubsets(2))
        covered(path) && covered(star)
      }
    ),
    check(
      "complementing-edges", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        val star = graphOn(four, edge(0, 1), edge(0, 2), edge(0, 3))
        val phi = Function.tryOfRelation(
          Relation.ofPairs(
            FiniteSet.empty[(Int, Int)].insert((0, 2)).insert((1, 0)).insert((2, 3)).insert((3, 1))
          )
        ).get
        val pairs = path.edges.toList.foldLeft(FiniteSet.empty[(FiniteSet.Set[Int], FiniteSet.Set[Int])]) {
          (acc, edge) =>
            val image = edge.toList.foldLeft(FiniteSet.empty[Int])((set, vertex) => set.insert(phi(vertex).get))
            acc.insert((edge, image))
        }
        val forward = Function.tryOfRelation(Relation.ofPairs(pairs)).get
        forward.isBijective(path.edges, Graph.complement(path).edges)
          && Enumeration.count(star.edges) == Enumeration.count(Graph.complement(star).edges)
          && Graph.complementing(star).toList.isEmpty
      }
    ),
    check(
      "degree-halves", {
        val four = stored.insert(3)
        val path = graphOn(four, edge(0, 1), edge(1, 2), edge(2, 3))
        val missing = Graph.complement(path)
        val one = graphOn(FiniteSet.singleton(0))
        val oneMissing = Graph.complement(one)
        path.vertices.toList.forall { vertex =>
          path.degree(vertex).get + missing.degree(vertex).get == path.vertices.remove(vertex).cardinality
        }
          && one.degree(0).get + oneMissing.degree(0).get == one.vertices.remove(0).cardinality
      }
    )
  )

  if !results.forall(identity) then sys.exit(1)
