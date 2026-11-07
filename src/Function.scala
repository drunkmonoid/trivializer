package trivializer

// A function is a relation that sends each domain element to one value.

object Function:
  opaque type Function[A, B] = Relation.Relation[A, B]

  given [A: Ordering, B: Ordering]: Ordering[Function[A, B]] with
    def compare(left: Function[A, B], right: Function[A, B]): Int =
      def walk(xs: List[(A, B)], ys: List[(A, B)]): Int =
        (xs, ys) match
          case (Nil, Nil) => 0
          case (Nil, _)   => -1
          case (_, Nil)   => 1
          case (x :: xt, y :: yt) =>
            val here = summon[Ordering[(A, B)]].compare(x, y)
            if here != 0 then here else walk(xt, yt)

      walk(Relation.pairs(relation(left)).toList, Relation.pairs(relation(right)).toList)

  def relation[A, B](function: Function[A, B]): Relation.Relation[A, B] = function

  def tryOfRelation[A: Ordering, B: Ordering](
      relation: Relation.Relation[A, B]
  ): Option[Function[A, B]] =
    def singleValued(pairs: List[(A, B)]): Boolean =
      pairs match
        case (a, _) :: (c, _) :: _ if summon[Ordering[A]].equiv(a, c) => false
        case _ :: rest                                                => singleValued(rest)
        case Nil                                                      => true

    if singleValued(Relation.pairs(relation).toList) then Some(relation) else None

  def identity[A: Ordering](set: FiniteSet.Set[A]): Function[A, A] =
    val pairs =
      set.toList.foldLeft(FiniteSet.empty[(A, A)])((acc, x) => FiniteSet.insert((x, x), acc))
    Relation.ofPairs(pairs)

  extension [A: Ordering, B](function: Function[A, B])
    def apply(x: A): Option[B] =
      Relation.pairs(relation(function)).toList.collectFirst {
        case (a, b) if summon[Ordering[A]].equiv(a, x) => b
      }

  // x |-> after(before x), when every value of before lands in the domain of after.
  extension [B: Ordering, C: Ordering](after: Function[B, C])
    def compose[A: Ordering](before: Function[A, B]): Option[Function[A, C]] =
      val middle = Relation.range(relation(before)).isSubset(Relation.domain(relation(after)))
      if !middle then None
      else
        val pairs =
          Relation.pairs(relation(before)).toList.foldLeft(FiniteSet.empty[(A, C)]) { (acc, pair) =>
            val (a, b) = pair
            after(b) match
              case Some(c) => FiniteSet.insert((a, c), acc)
              case None    => acc
          }
        Some(Relation.ofPairs(pairs))

  // Predicates on one function and its stated domain and codomain.
  extension [A: Ordering, B: Ordering](function: Function[A, B])
    private def listed: List[(A, B)] = Relation.pairs(relation(function)).toList

    private def totalOn(domain: FiniteSet.Set[A], codomain: FiniteSet.Set[B]): Boolean =
      val inside = listed.forall { (a, b) =>
        domain.contains(a) && codomain.contains(b)
      }
      inside && domain.toList.forall(a => function(a).isDefined)

    def isInjective(domain: FiniteSet.Set[A], codomain: FiniteSet.Set[B]): Boolean =
      val images = listed
      totalOn(domain, codomain) && images.forall { (a, b) =>
        images.forall { (c, d) =>
          if summon[Ordering[B]].equiv(b, d) then summon[Ordering[A]].equiv(a, c) else true
        }
      }

    def isSurjective(domain: FiniteSet.Set[A], codomain: FiniteSet.Set[B]): Boolean =
      val images = listed
      totalOn(domain, codomain) && codomain.toList.forall { b =>
        images.exists((_, d) => summon[Ordering[B]].equiv(b, d))
      }

    def isBijective(domain: FiniteSet.Set[A], codomain: FiniteSet.Set[B]): Boolean =
      isInjective(domain, codomain) && isSurjective(domain, codomain)
