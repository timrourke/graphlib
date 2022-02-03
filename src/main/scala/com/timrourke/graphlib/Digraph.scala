package com.timrourke.graphlib

import scala.collection.mutable

trait Edge[V] {
  def from: V

  def to: V
}

case class SimpleEdge[V](from: V, to: V) extends Edge[V]

class DigraphHasCyclesException
  extends RuntimeException(
    "Digraph cannot be sorted topologically: Cycles detected, no nodes exist without incoming edges"
  )

trait Graph[V] {
  def edges: Seq[Edge[V]]

  def adjacencyList: Map[V, List[V]]

  def depthFirstSearchPreOrder(from: Seq[V], visitor: V => Unit): Unit = {
    val stack = mutable.Stack.empty[V]
    val visited = mutable.Set.empty[V]

    def visit(from: V): Unit = {
      if (visited.contains(from)) {
        return
      }

      visitor(from)

      //noinspection DuplicatedCode
      visited.add(from)

      adjacencyList.get(from).foreach(vertices => {
        vertices.foreach(stack.push)
      })

      while (stack.nonEmpty) {
        visit(stack.pop())
      }
    }

    from.foreach(visit)
  }

  def depthFirstSearchPostOrder(from: Seq[V], visitor: V => Unit): Unit = {
    val stack = mutable.Stack.empty[V]
    val visited = mutable.Set.empty[V]

    def visit(from: V): Unit = {
      if (visited.contains(from)) {
        return
      }

      //noinspection DuplicatedCode
      visited.add(from)

      adjacencyList.get(from).foreach(vertices => {
        vertices.foreach(stack.push)
      })

      while (stack.nonEmpty) {
        visit(stack.pop())
      }

      visitor(from)
    }

    from.foreach(visit)
  }
}

case class UndirectedGraph[V](edges: Seq[Edge[V]])(implicit ord: Ordering[V])
  extends Graph[V] {

  lazy val adjacencyList: Map[V, List[V]] = edges.foldLeft(Map.empty[V, List[V]])((acc, edge) => {
    val fromNeighbors = edge.to :: acc.getOrElse(edge.from, List.empty[V])
    val toNeighbors = edge.from :: acc.getOrElse(edge.to, List.empty[V])

    acc + (edge.from -> fromNeighbors) + (edge.to -> toNeighbors)
  }).map { case k -> v => k -> v.sorted }

}

case class Digraph[V](edges: Seq[Edge[V]])(implicit ord: Ordering[V])
  extends Graph[V] {

  lazy val adjacencyList: Map[V, List[V]] = edges.foldLeft(Map.empty[V, List[V]])((acc, edge) => {
    val fromNeighbors = edge.to :: acc.getOrElse(edge.from, List.empty[V])

    acc + (edge.from -> fromNeighbors)
  }).map { case k -> v => k -> v.sorted }

  lazy val nodesWithoutIncomingEdges: Set[V] = adjacencyList.keySet
    .diff(adjacencyList.values.flatten.toSet)

  def topologicalSort(): List[V] = {
    if (nodesWithoutIncomingEdges.isEmpty) {
      throw new DigraphHasCyclesException()
    }

    val result = mutable.ListBuffer.empty[V]

    depthFirstSearchPostOrder(nodesWithoutIncomingEdges.toList, v => {
      result.prepend(v)
    })

    result.result()
  }

  def weaklyConnectedComponents(): List[Digraph[V]] = {
    val undirectedGraph = UndirectedGraph(edges)
    val visited = mutable.Set.empty[Edge[V]]

    undirectedGraph
      .edges
      .foldLeft(List.empty[Digraph[V]]){ case (acc, edge) =>
        if (visited.contains(edge)) {
          println("dupe detected")
          acc
        } else {
          val buf = mutable.ListBuffer.empty[Edge[V]]

          undirectedGraph.depthFirstSearchPreOrder(List(edge.from, edge.to), from => {
            adjacencyList.get(from).foreach(toNeighbors => {
              toNeighbors.foreach(to => {
                val edge = SimpleEdge(from, to)
                visited.add(edge)
                buf.addOne(edge)
              })
            })
          })

          Digraph(buf.result()) :: acc
        }
      }
  }
}
