package trivializer

import scala.annotation.targetName

// A finite set stored as a sorted list of distinct elements.

enum Tagged[+A, +B]:
  case Left(value: A)
  case Right(value: B)

object Tagged:
  given [A: Ordering, B: Ordering]: Ordering[Tagged[A, B]] with
    def compare(x: Tagged[A, B], y: Tagged[A, B]): Int =
      (x, y) match
        case (Tagged.Left(a), Tagged.Left(b))   => summon[Ordering[A]].compare(a, b)
        case (Tagged.Right(a), Tagged.Right(b)) => summon[Ordering[B]].compare(a, b)
        case (Tagged.Left(_), Tagged.Right(_))  => -1
        case (Tagged.Right(_), Tagged.Left(_))  => 1

object FiniteSet:
  opaque type Set[A] = List[A]

  def empty[A]: Set[A] = Nil

  given [A: Ordering]: Ordering[Set[A]] with
    def compare(left: Set[A], right: Set[A]): Int =
      def walk(xs: List[A], ys: List[A]): Int =
        (xs, ys) match
          case (Nil, Nil) => 0
          case (Nil, _)   => -1
          case (_, Nil)   => 1
          case (x :: xt, y :: yt) =>
            val here = summon[Ordering[A]].compare(x, y)
            if here != 0 then here else walk(xt, yt)

      val xs: List[A] = left
      val ys: List[A] = right
      walk(xs, ys)

  private def insertInto[A: Ordering](x: A, xs: List[A]): List[A] =
    xs match
      case Nil                                    => x :: Nil
      case y :: _ if summon[Ordering[A]].equiv(x, y) => xs
      case y :: _ if summon[Ordering[A]].lt(x, y)    => x :: xs
      case y :: rest                              => y :: insertInto(x, rest)

  def singleton[A: Ordering](x: A): Set[A] = insertInto(x, Nil)

  def insert[A: Ordering](x: A, set: Set[A]): Set[A] = insertInto(x, set)

  private def containsIn[A: Ordering](x: A, xs: List[A]): Boolean =
    xs match
      case Nil                                    => false
      case y :: _ if summon[Ordering[A]].lt(x, y)    => false
      case y :: _ if summon[Ordering[A]].equiv(x, y) => true
      case _ :: rest                              => containsIn(x, rest)

  private def removeFrom[A: Ordering](x: A, xs: List[A]): List[A] =
    xs match
      case Nil                                    => Nil
      case y :: rest if summon[Ordering[A]].equiv(x, y) => rest
      case y :: _ if summon[Ordering[A]].lt(x, y)       => xs
      case y :: rest                              => y :: removeFrom(x, rest)

  private def equalLists[A: Ordering](left: List[A], right: List[A]): Boolean =
    (left, right) match
      case (Nil, Nil)                                 => true
      case (x :: xs, y :: ys) if summon[Ordering[A]].equiv(x, y) =>
        equalLists(xs, ys)
      case _                                          => false

  private def subsetLists[A: Ordering](candidate: List[A], ambient: List[A]): Boolean =
    (candidate, ambient) match
      case (Nil, _) => true
      case (_, Nil) => false
      case (x :: xs, y :: ys) if summon[Ordering[A]].equiv(x, y) =>
        subsetLists(xs, ys)
      case (x :: _, y :: ys) if summon[Ordering[A]].lt(y, x) =>
        subsetLists(candidate, ys)
      case _ => false

  private def unionLists[A: Ordering](left: List[A], right: List[A]): List[A] =
    (left, right) match
      case (Nil, _) => right
      case (_, Nil) => left
      case (x :: xs, y :: ys) if summon[Ordering[A]].equiv(x, y) =>
        x :: unionLists(xs, ys)
      case (x :: xs, y :: _) if summon[Ordering[A]].lt(x, y) =>
        x :: unionLists(xs, right)
      case (_, y :: ys) =>
        y :: unionLists(left, ys)

  private def intersectLists[A: Ordering](left: List[A], right: List[A]): List[A] =
    (left, right) match
      case (Nil, _) | (_, Nil) => Nil
      case (x :: xs, y :: ys) if summon[Ordering[A]].equiv(x, y) =>
        x :: intersectLists(xs, ys)
      case (x :: xs, y :: _) if summon[Ordering[A]].lt(x, y) =>
        intersectLists(xs, right)
      case (_, _ :: ys) =>
        intersectLists(left, ys)

  private def differenceLists[A: Ordering](left: List[A], right: List[A]): List[A] =
    (left, right) match
      case (Nil, _) => Nil
      case (_, Nil) => left
      case (x :: xs, y :: ys) if summon[Ordering[A]].equiv(x, y) =>
        differenceLists(xs, ys)
      case (x :: xs, y :: _) if summon[Ordering[A]].lt(x, y) =>
        x :: differenceLists(xs, right)
      case (_, _ :: ys) =>
        differenceLists(left, ys)

  def product[A: Ordering, B: Ordering](left: Set[A], right: Set[B]): Set[(A, B)] =
    left.foldLeft(empty[(A, B)]) { (acc, x) =>
      right.foldLeft(acc)((acc, y) => insertInto((x, y), acc))
    }

  def fst[A, B](pair: (A, B)): A = pair._1

  def snd[A, B](pair: (A, B)): B = pair._2

  def injectLeft[A, B](x: A): Tagged[A, B] = Tagged.Left(x)

  def injectRight[A, B](y: B): Tagged[A, B] = Tagged.Right(y)

  def sum[A: Ordering, B: Ordering](left: Set[A], right: Set[B]): Set[Tagged[A, B]] =
    unionLists(map(injectLeft[A, B], left), map(injectRight[A, B], right))

  def map[A, B: Ordering](f: A => B, set: Set[A]): Set[B] =
    set.foldLeft(empty[B])((acc, x) => insertInto(f(x), acc))

  // Each element is kept or dropped. Both results are stored.
  def power[A: Ordering](set: Set[A]): Set[Set[A]] =
    val items: List[A] = set
    items.foldLeft(singleton(empty[A])) { (families, element) =>
      val grown = map((subset: Set[A]) => insert(element, subset), families)
      unionLists(families, grown)
    }

  extension [A](set: Set[A])
    def toList: List[A] = set
    def cardinality: Int = set.length

  extension [A: Ordering](set: Set[A])
    def insert(x: A): Set[A] = insertInto(x, set)
    def contains(x: A): Boolean = containsIn(x, set)
    def remove(x: A): Set[A] = removeFrom(x, set)
    def equal(other: Set[A]): Boolean = equalLists(set, other)
    def isSubset(ambient: Set[A]): Boolean = subsetLists(set, ambient)
    @targetName("unionExt")
    def union(other: Set[A]): Set[A] = unionLists(set, other)
    def intersect(other: Set[A]): Set[A] = intersectLists(set, other)
    def difference(other: Set[A]): Set[A] = differenceLists(set, other)
    def filter(predicate: A => Boolean): Set[A] =
      val items: List[A] = set
      items.filter(predicate)
    def map[B: Ordering](f: A => B): Set[B] = FiniteSet.map(f, set)
    @targetName("powerExt")
    def power: Set[Set[A]] = FiniteSet.power(set)
