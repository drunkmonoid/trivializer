package trivializer

import scala.annotation.targetName

// A relation is a set of pairs. Domain and range are the two components.

object Relation:
  opaque type Relation[A, B] = FiniteSet.Set[(A, B)]

  def empty[A, B]: Relation[A, B] = FiniteSet.empty[(A, B)]

  def ofPairs[A, B](pairs: FiniteSet.Set[(A, B)]): Relation[A, B] = pairs

  def add[A: Ordering, B: Ordering](relation: Relation[A, B], a: A, b: B): Relation[A, B] =
    FiniteSet.insert((a, b), relation)

  def pairs[A, B](relation: Relation[A, B]): FiniteSet.Set[(A, B)] = relation

  def domain[A: Ordering, B: Ordering](relation: Relation[A, B]): FiniteSet.Set[A] =
    FiniteSet.map((pair: (A, B)) => pair._1, relation)

  def range[A: Ordering, B: Ordering](relation: Relation[A, B]): FiniteSet.Set[B] =
    FiniteSet.map((pair: (A, B)) => pair._2, relation)

  extension [A: Ordering, B: Ordering](relation: Relation[A, B])
    @targetName("addExt")
    def add(a: A, b: B): Relation[A, B] = Relation.add(relation, a, b)

    @targetName("pairsExt")
    def pairs: FiniteSet.Set[(A, B)] = Relation.pairs(relation)

    @targetName("domainExt")
    def domain: FiniteSet.Set[A] = Relation.domain(relation)

    @targetName("rangeExt")
    def range: FiniteSet.Set[B] = Relation.range(relation)
