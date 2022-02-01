package com.timrourke.graphlib

import scala.collection.mutable

trait Edge[V] {
  def from: V

  def to: V
}

case class SimpleEdge[V](from: V, to: V) extends Edge[V]

class DigraphInvalidHasCycles(message: String) extends RuntimeException(message)

case class Digraph[V](edges: Seq[Edge[V]])(implicit ord: Ordering[V]) {

  val adjacencyList: Map[V, List[V]] = edges
    .groupBy(_.from)
    .map {
      case (fromVertex, toEdges) => fromVertex -> toEdges.toList.map(_.to).sorted
    }

  val nodesWithoutIncomingEdges: Set[V] = adjacencyList.keySet
    .diff(adjacencyList.values.flatten.toSet)

  if (nodesWithoutIncomingEdges.isEmpty) {
    throw new DigraphInvalidHasCycles("Digraph Invalid: Cycles detected, no nodes exist without incoming edges")
  }

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
