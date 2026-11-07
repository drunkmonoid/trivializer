package trivializer

// Vertices, and edges that are 2-element subsets of those vertices. A loop is not an edge.

object Graph:
  opaque type Graph[A] = (FiniteSet.Set[A], FiniteSet.Set[FiniteSet.Set[A]])

  given [A: Ordering]: Ordering[Graph[A]] with
    def compare(left: Graph[A], right: Graph[A]): Int =
      val byVertices = summon[Ordering[FiniteSet.Set[A]]].compare(left.vertices, right.vertices)
      if byVertices != 0 then byVertices
      else summon[Ordering[FiniteSet.Set[FiniteSet.Set[A]]]].compare(left.edges, right.edges)

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

    def isConnected(using Ordering[A]): Boolean =
      vertices.toList match
        case Nil        => true
        case start :: _ => reachable(start).get.equal(vertices)

    def isTree(using Ordering[A]): Boolean =
      isConnected && Enumeration.count(edges) + 1 == Enumeration.count(vertices)

  def pruferEncode[A: Ordering](graph: Graph[A]): Option[Sequence.Sequence[A]] =
    if !graph.isTree then None
    else if graph.vertices.cardinality < 2 then Some(Sequence.empty[A])
    else
      def step(
          vertices: FiniteSet.Set[A],
          edges: FiniteSet.Set[FiniteSet.Set[A]],
          built: List[A]
      ): List[A] =
        if vertices.cardinality == 2 then built.reverse
        else
          val leaf = vertices.toList.find { vertex =>
            edges.toList.count(edge => edge.contains(vertex)) == 1
          }.get
          val edge = edges.toList.find(edge => edge.contains(leaf)).get
          val neighbor = edge.toList.find(vertex => !summon[Ordering[A]].equiv(vertex, leaf)).get
          step(vertices.remove(leaf), edges.remove(edge), neighbor :: built)

      val letters = step(graph.vertices, graph.edges, Nil)
      Some(letters.foldRight(Sequence.empty[A])(Sequence.cons))

  def pruferDecode[A: Ordering](
      vertices: FiniteSet.Set[A],
      word: Sequence.Sequence[A]
  ): Option[Graph[A]] =
    val letters = word.toList
    if vertices.cardinality < 2 then
      if letters.isEmpty then tryOf(vertices, FiniteSet.empty[FiniteSet.Set[A]]) else None
    else if letters.length + 2 != vertices.cardinality || !letters.forall(vertices.contains) then None
    else
      val same = summon[Ordering[A]].equiv
      val degrees = vertices.toList.map(vertex => (vertex, 1 + letters.count(letter => same(letter, vertex))))
      def take(
          code: List[A],
          degrees: List[(A, Int)],
          edges: FiniteSet.Set[FiniteSet.Set[A]]
      ): FiniteSet.Set[FiniteSet.Set[A]] =
        code match
          case Nil =>
            val last = degrees.filter((_, degree) => degree == 1).map((vertex, _) => vertex)
            edges.insert(FiniteSet.empty[A].insert(last.head).insert(last.tail.head))
          case symbol :: rest =>
            val leaf = degrees.filter((_, degree) => degree == 1).map((vertex, _) => vertex).min
            val next = degrees.map { (vertex, degree) =>
              if same(vertex, leaf) || same(vertex, symbol) then (vertex, degree - 1) else (vertex, degree)
            }
            take(rest, next, edges.insert(FiniteSet.empty[A].insert(leaf).insert(symbol)))

      tryOf(vertices, take(letters, degrees, FiniteSet.empty))

  def labeledTrees[A: Ordering](vertices: FiniteSet.Set[A]): FiniteSet.Set[Graph[A]] =
    if vertices.cardinality < 2 then
      FiniteSet.singleton(tryOf(vertices, FiniteSet.empty[FiniteSet.Set[A]]).get)
    else
      val length = vertices.cardinality - 2
      Sequence.words(vertices, length).toList.foldLeft(FiniteSet.empty[Graph[A]]) { (acc, word) =>
        pruferDecode(vertices, word) match
          case Some(tree) => acc.insert(tree)
          case None       => acc
      }
