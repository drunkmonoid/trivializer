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

## Run

This project targets Scala 3.9. Install [Scala CLI](https://scala-cli.virtuslab.org/), then from this directory:

```bash
scala-cli run .
```

Exit code 0 means every invariant above holds. Exit code 1 means one of them does not.

## Next

The next chunk enumerates every object of a given kind: all subsets, all words, all permutations, all partitions. A count is the cardinality of that enumeration. An identity is two enumerations with the same cardinality. That chunk is not started here.

## License

[MIT](LICENSE). Copyright 2026 drunkmonoid.
