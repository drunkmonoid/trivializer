package trivializer

// Vertices, and edges that are 2-element subsets of those vertices. A loop is not an edge.

object Graph:
  opaque type Graph[A] = (FiniteSet.Set[A], FiniteSet.Set[FiniteSet.Set[A]])

  def tryOf[A: Ordering](
      vertices: FiniteSet.Set[A],
      edges: FiniteSet.Set[FiniteSet.Set[A]]
  ): Option[Graph[A]] =
    val edgesFit = edges.toList.forall(edge => edge.cardinality == 2 && edge.isSubset(vertices))
    if edgesFit then Some((vertices, edges)) else None

  private def parts[A](
      graph: Graph[A]
  ): (FiniteSet.Set[A], FiniteSet.Set[FiniteSet.Set[A]]) = graph

  extension [A](graph: Graph[A])
    def vertices: FiniteSet.Set[A] =
      val (vertices, _) = parts(graph)
      vertices

    def edges: FiniteSet.Set[FiniteSet.Set[A]] =
      val (_, edges) = parts(graph)
      edges

    def neighbors(vertex: A)(using Ordering[A]): Option[FiniteSet.Set[A]] =
      if !vertices.contains(vertex) then None
      else
        Some(edges.toList.foldLeft(FiniteSet.empty[A]) { (acc, edge) =>
          if !edge.contains(vertex) then acc
          else
            edge.toList.foldLeft(acc) { (acc, other) =>
              if summon[Ordering[A]].equiv(other, vertex) then acc else acc.insert(other)
            }
        })

    def degree(vertex: A)(using Ordering[A]): Option[Int] =
      if !vertices.contains(vertex) then None
      else
        val incident = edges.filter(edge => edge.contains(vertex))
        Some(Enumeration.count(incident))

    def tryWalk(steps: Sequence.Sequence[A])(using Ordering[A]): Option[Sequence.Sequence[A]] =
      def linked(current: A, rest: List[A]): Boolean =
        rest match
          case Nil => true
          case next :: tail =>
            val step = FiniteSet.empty[A].insert(current).insert(next)
            edges.contains(step) && linked(next, tail)

      val letters = steps.toList
      if letters.nonEmpty && letters.forall(vertices.contains) && linked(letters.head, letters.tail) then
        Some(steps)
      else None

    def reachable(start: A)(using Ordering[A]): Option[FiniteSet.Set[A]] =
      if !vertices.contains(start) then None
      else
        def walk(frontier: List[A], seen: FiniteSet.Set[A]): FiniteSet.Set[A] =
          frontier match
            case Nil => seen
            case current :: rest =>
              if seen.contains(current) then walk(rest, seen)
              else walk(neighbors(current).get.toList ++ rest, seen.insert(current))

        Some(walk(List(start), FiniteSet.empty[A]))
