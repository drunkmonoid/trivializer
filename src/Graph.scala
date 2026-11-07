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
