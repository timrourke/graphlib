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

case class Digraph[V](edges: Seq[Edge[V]])(implicit ord: Ordering[V]) {

  lazy val adjacencyList: Map[V, List[V]] = edges
    .groupBy(_.from)
    .map {
      case (fromVertex, toEdges) => fromVertex -> toEdges.toList.map(_.to).sorted
    }

  lazy val nodesWithoutIncomingEdges: Set[V] = adjacencyList.keySet
    .diff(adjacencyList.values.flatten.toSet)

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
