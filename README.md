# trivializer

We assume nothing and start from scratch. Math for Peace.

This is the foundation. Each module stores one object, or one operation on objects already stored. A number that appears is a length or a multiplicity that was stored. The runner builds one example of each object and checks its invariant.

Read the modules in this order:

1. `FiniteSet` stores a finite set as a sorted list of distinct elements. `insert` and `remove` change it. `cardinality` is how many elements are stored. `equal` and `isSubset` walk the elements. `union`, `intersect`, and `difference` build a new set from two stored ones. `filter` keeps some of them. `map` sends each element somewhere, and the image is a set, so duplicates collapse. `product` stores a pair for each element of one set with each element of another. `fst` and `snd` read a pair back. `sum` tags one side `Left` and the other `Right`, so a value that lives on both sides is stored twice.
2. `Relation` is a set of pairs. `domain` and `range` are the sets of first and second components.
3. `Function` is a relation that gives each input one output. `apply` reads that output. `identity` sends every element of a set to itself. `compose` walks the pairs and builds the chained function when every middle value lands in the next domain. `isInjective`, `isSurjective`, and `isBijective` look at one function together with a stated domain and codomain.
4. `Sequence` keeps order and may repeat an element, so it is not a set. `cons` adds at the front. `item` reads one place. `length` is how many places are stored. `words` builds the set of sequences of length `k` over an alphabet by taking products.
5. `Multiset` stores a multiplicity on an element. `add` increases that stored count. `multiplicity` reads it. `support` forgets the counts and returns a finite set.
6. `Subset` pairs an ambient set with the chosen elements. Construction succeeds only when every chosen element lies in the ambient set.
7. `Combination` is one witnessed subset whose cardinality is a given `k`. The value is that subset.
8. `Permutation` is one sequence that contains each element of the ambient set once, hence one bijection. `compose` and `inverse` act on that value. `at` and `act` apply it to one element and to one subset. `orbit` walks a single element.
9. `Partition` is a set of nonempty blocks. Construction succeeds when the blocks are pairwise disjoint and their union is the ambient set.

The enumeration layer stores every object of a given kind. A count is the cardinality of a stored family. An identity is two stored families with the same count, or one stored bijection between them. A closed form is not the definition.

Read these next:

1. `FiniteSet.power` keeps or drops each element and stores both results. `kSubsets` keeps the subsets whose stored size is `k`.
2. `Enumeration` stores the permutations, the set-partitions, and the functions from one set to another. Injections, surjections, and bijections are the functions that pass the predicate already stored on `Function`. `count` reads the cardinality of a family. `subsetsBySize` tags each subset by that size and covers the power set. `complement` sends a k-subset to its complement. `subsetToWord` and `wordToSubset` match a subset with the binary word of its membership. `permutationToMap` and `mapToPermutation` match a permutation with the bijection it stores. `pascalSplit` sends each k-subset to one side of the tagged union. `splitWord` and `joinWord` match a word with its last letter and its prefix.
3. `Graph` stores vertices and edges that are 2-subsets of those vertices. `neighbors` is the other ends of the edges at one vertex. `degree` counts those edges. `tryWalk` accepts a sequence whose consecutive pairs are stored edges. `reachable` walks neighbors from one vertex. `isConnected` asks whether that walk meets every vertex. `isTree` asks whether the graph is connected and the edge count is one less than the vertex count. `pruferEncode` and `pruferDecode` turn one tree into a word and back. `labeledTrees` stores every decoding of a word of length `|V| - 2` over the vertex set.

An unlabeled graph is a labeled graph standing for every relabeling of itself. The representative is the least relabeling. The census stores one representative per class. A count is still the cardinality of a stored family.

Read these next:

1. `relabel` carries each edge along a bijection of the vertices. `isIsomorphism` accepts that bijection when the carried edges match the target. `isomorphisms` stores every such bijection, and `automorphisms` keeps the ones from a graph to itself. `vertexOrbit` and `edgeOrbit` are the images of one vertex and one edge under those automorphisms.
2. `relabelings` stores every graph reached that way. `canonical` keeps the least of them. `sameUnlabeled` asks whether two graphs have the same canonical graph.
3. `allGraphs` stores every edge set drawn from the 2-subsets of a vertex set. `edgeGraph` matches each of those subsets with its graph. `census` keeps one canonical graph per class. `orbitSection` and `orbitSplit` split a vertex bijection into a relabeling and a leftover automorphism. `connectedCensus` and `treeCensus` keep the connected graphs and the trees. `canonicalTrees` is the same tree set read off the labeled trees.
4. `degreeSequence` lists the stored degrees in vertex order. `sortedDegreeSequence` is that sequence sorted. `flags` stores a vertex with each edge that contains it. On four vertices the sorted degree sequence and the canonical graph split the stored graphs the same way. On five vertices a path and a triangle disjoint from an edge share a degree sequence and differ as canonical graphs.
5. `complement` stores the potential edges that were missing. It commutes with relabeling and permutes the three-vertex census. The complements of that five-vertex pair stay connected and stay apart. `ontoCanonical` matches the automorphisms with the isomorphisms onto the canonical graph.

A set of vertices determines the edges that lie inside it. A subgraph is that set together with those edges. Nothing else is chosen.

Read these next:

1. `induced` keeps the edges that lie inside a chosen set of vertices. Choosing every vertex returns the graph. It commutes with `relabel` and with `complement`.
2. `component` is the induced subgraph on the vertices reachable from one vertex. `components` stores those reachable sets, and they form a `Partition` of the vertices. Every edge lies inside one block. On four vertices a graph is connected when there is one block.
3. `isForest` asks whether the edge count and the block count add up to the vertex count. On four vertices a forest is a graph whose components are trees, and a tree is a connected forest.
4. `card` drops one vertex and keeps the induced subgraph on the rest. `deck` stores the canonical cards as a multiset, one card per vertex. It is kept after a relabeling. On four vertices the deck and the canonical graph split the stored graphs the same way. On two vertices the edge and the missing edge share a deck and differ as canonical graphs.

A graph is self-complementary when a stored bijection carries it onto its complement. An equal edge count is not that witness.

Read these next:

1. `isSelfComplementary` asks whether a graph and its complement are the same unlabeled graph. The path on four vertices passes. The star fails. One vertex passes. Two and three vertices do not.
2. `selfComplementary` stores every such labeled graph. `selfComplementaryCensus` keeps one canonical graph per class. On four vertices that census is the canonical path. On one vertex it is that graph. On two and three vertices it is empty.
3. `complementing` stores every bijection onto the complement. The square of each one for the path is an automorphism. `edgeHalves` tags the edges of a graph and of its complement and matches them with the potential edges. `edgeOrbits` stores the orbits of those potential edges under a complementing bijection. `mapOrbit` is the orbit of one vertex under that bijection.
4. The path's complement passes, and its deck matches. A three-vertex piece of the path fails. Each isolated vertex passes while the two-vertex graph fails. On five vertices the five-cycle and the bull pass and differ. The path on five vertices fails.

## Run

This project targets Scala 3.9. Install [Scala CLI](https://scala-cli.virtuslab.org/), then from this directory:

```bash
scala-cli run .
```

Exit code 0 means every invariant above holds. Exit code 1 means one of them does not.

## Next

Homomorphisms. A map of vertices that sends every edge to an edge. An isomorphism is a bijective homomorphism whose inverse is also a homomorphism.

## License

[MIT](LICENSE). Copyright 2026 drunkmonoid.
