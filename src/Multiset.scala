package trivializer

import scala.annotation.targetName

// A multiset stores a multiplicity on each element. The support forgets those counts.

object Multiset:
  opaque type Multiset[A] = List[(A, Int)]

  def empty[A]: Multiset[A] = Nil

  def add[A: Ordering](element: A, multiset: Multiset[A]): Multiset[A] =
    def insert(xs: List[(A, Int)]): List[(A, Int)] =
      xs match
        case Nil =>
          (element, 1) :: Nil
        case (y, n) :: rest if summon[Ordering[A]].equiv(element, y) =>
          (y, n + 1) :: rest
        case (y, _) :: _ if summon[Ordering[A]].lt(element, y) =>
          (element, 1) :: xs
        case head :: rest =>
          head :: insert(rest)

    val items: List[(A, Int)] = multiset
    insert(items)

  extension [A: Ordering](multiset: Multiset[A])
    @targetName("addExt")
    def add(element: A): Multiset[A] = Multiset.add(element, multiset)

    def multiplicity(element: A): Int =
      def find(xs: List[(A, Int)]): Int =
        xs match
          case Nil =>
            0
          case (y, _) :: _ if summon[Ordering[A]].lt(element, y) =>
            0
          case (y, n) :: _ if summon[Ordering[A]].equiv(element, y) =>
            n
          case _ :: rest =>
            find(rest)

      val items: List[(A, Int)] = multiset
      find(items)

    def support: FiniteSet.Set[A] =
      val items: List[(A, Int)] = multiset
      items.foldLeft(FiniteSet.empty[A])((acc, pair) => acc.insert(pair._1))
