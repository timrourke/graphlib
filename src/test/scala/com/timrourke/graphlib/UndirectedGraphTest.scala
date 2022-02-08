package com.timrourke.graphlib

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class UndirectedGraphTest extends AnyFunSpec with Matchers {

  describe("UndirectedGraph") {

    it("should construct an adjacency list") {
      val edges = List(
        SimpleEdge("A", "B"),
        SimpleEdge("B", "C"),
        SimpleEdge("A", "C"),
      )

      val graph = UndirectedGraph(edges)

      graph.adjacencyList shouldBe Map(
        "A" -> List("B", "C"),
        "B" -> List("A", "C"),
        "C" -> List("A", "B"),
      )
    }
  }

  it("should perform pre-order depth-first search") {
    val edges = List(
      SimpleEdge("A", "B"),
      SimpleEdge("B", "C"),
      SimpleEdge("A", "C"),
    )

    val graph = UndirectedGraph(edges)

    val buf = mutable.ListBuffer.empty[String]

    graph.depthFirstSearchPreOrder(List("A"), v => buf.addOne(v))

    buf.result() shouldBe List("A", "B", "C")
  }

  it("should perform post-order depth-first search") {
    val edges = List(
      SimpleEdge("A", "B"),
      SimpleEdge("B", "C"),
      SimpleEdge("A", "C"),
    )

    val graph = UndirectedGraph(edges)

    val buf = mutable.ListBuffer.empty[String]

    graph.depthFirstSearchPostOrder(List("A"), v => buf.addOne(v))

    buf.result() shouldBe List("B", "C", "A")
  }
}
