package com.timrourke.graphlib

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class DigraphTest extends AnyFunSpec with Matchers {

  private val edges = List(
    SimpleEdge(0, 1),
    SimpleEdge(0, 5),
    SimpleEdge(0, 6),
    SimpleEdge(2, 0),
    SimpleEdge(2, 3),
    SimpleEdge(3, 5),
    SimpleEdge(5, 4),
    SimpleEdge(6, 4),
    SimpleEdge(6, 9),
    SimpleEdge(7, 6),
    SimpleEdge(8, 7),
    SimpleEdge(9, 10),
    SimpleEdge(9, 11),
    SimpleEdge(9, 12),
    SimpleEdge(11, 12),
  )

  describe("Digraph") {

    val graph = Digraph(edges)

    it("should construct an adjacency list") {
      graph.adjacencyList shouldBe Map(
        0 -> List(1, 5, 6),
        2 -> List(0, 3),
        3 -> List(5),
        5 -> List(4),
        6 -> List(4, 9),
        7 -> List(6),
        8 -> List(7),
        9 -> List(10, 11, 12),
        11 -> List(12),
      )
    }

    it("should perform pre-order depth-first search") {
      val buf = mutable.ListBuffer.empty[Int]

      graph.depthFirstSearchPreOrder(List(0), (v: Int) => {
        buf.addOne(v)
      })

      buf.result() shouldBe List(
        0,
        6,
        9,
        12,
        11,
        10,
        4,
        5,
        1,
      )
    }

    it("should perform post-order depth-first search") {
      val buf = mutable.ListBuffer.empty[Int]

      graph.depthFirstSearchPostOrder(List(0), (v: Int) => {
        buf.addOne(v)
      })

      buf.result() shouldBe List(
        1,
        5,
        4,
        10,
        11,
        12,
        9,
        6,
        0,
      )
    }

    it("should topologically sort tiny digraph") {
      val graph = Digraph(List(SimpleEdge("A", "B")))

      graph.topologicalSort() shouldBe List("A", "B")
    }

    it("should topologically sort") {
      val actual = graph.topologicalSort()

      actual shouldBe List(
        8,
        7,
        2,
        3,
        5,
        4,
        0,
        6,
        9,
        12,
        11,
        10,
        1
      )
    }

    it("should detect cycles when attempting to topologically sort") {
      val edgesWithCycles = List(
        SimpleEdge("A", "B"),
        SimpleEdge("B", "C"),
        SimpleEdge("C", "A"),
      )

      intercept[DigraphHasCyclesException](Digraph(edgesWithCycles).topologicalSort())
    }
  }
}
