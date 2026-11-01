# trivializer

We assume nothing and start from scratch. Math for Peace.

Phase 10 of the foundation. A finite set from [Drunk Monoid](https://github.com/drunkmonoid). Elements go in through `insert` and leave through `remove`. What comes back is those elements, sorted, with duplicates gone. `cardinality` is how many elements are stored. `equal` and `isSubset` walk the elements one at a time. `union`, `intersect`, and `difference` build a new set from two stored ones. `filter` keeps some of them. `map` sends each element somewhere, and the image is a set, so duplicates collapse. `product` stores a pair for each element of one set with each element of another. `fst` and `snd` read a pair back. The number of pairs is how many were stored. `sum` tags one side `Left` and the other `Right`, so a value that lives on both sides is stored twice. That is a different construction from `union`. A relation is a set of pairs. `domain` and `range` are the sets of first and second components. A function is a relation that gives each input one output. `apply` reads that output. `identity` sends every element of a set to itself. `compose` walks those pairs and builds the chained function when every middle value lands in the next domain. This is the ground layer. Counting formulas come later.

## Run

This project targets Scala 3.9. Install [Scala CLI](https://scala-cli.virtuslab.org/), then from this directory:

```bash
scala-cli run .
```

Exit code 0 means storage, size, comparison, the boolean operations, the image, the product, the sum, relations, functions, and composition all match. Exit code 1 means they do not.

## License

[MIT](LICENSE). Copyright 2026 drunkmonoid.
