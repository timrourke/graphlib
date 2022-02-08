package com.timrourke.graphlib

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
        1,
        5,
        4,
        6,
        9,
        10,
        11,
        12,
      )
    }

    it("should perform pre-order depth-first search (2)") {
      val graph = Digraph(List(
        SimpleEdge(0, 1),
        SimpleEdge(1, 2),
        SimpleEdge(0, 2),
        SimpleEdge(2, 3),
        SimpleEdge(2, 0),
        SimpleEdge(3, 3),
      ))

      val buf = ListBuffer.empty[Int]

      graph.depthFirstSearchPreOrder(List(1), buf.addOne)

      buf.result() shouldBe List(1, 2, 0, 3)
    }

    it("should perform post-order depth-first search") {
      val graph = Digraph[Int](List(
        SimpleEdge(1, 3),
        SimpleEdge(1, 2),
        SimpleEdge(2, 4),
        SimpleEdge(2, 5),
      ))

      val buf = mutable.ListBuffer.empty[Int]

      graph.depthFirstSearchPostOrder(List(1), buf.addOne)

      buf.result() shouldBe List(
        4,
        5,
        2,
        3,
        1,
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

    it("should compute weakly connected components when only one component exists") {
      val actual = graph.weaklyConnectedComponents()

      actual.map(_.edges.toSet) shouldBe List(edges.toSet)
    }

    it("should detect multiple weakly connected components") {
      val edges = List(
        SimpleEdge("A", "B"),
        SimpleEdge("C", "A"),
        SimpleEdge("D", "E"),
        SimpleEdge("F", "B"),
      )

      val graph = Digraph(edges)

      val actual = graph.weaklyConnectedComponents()

      actual.map(_.edges.toSet).toSet shouldBe Set(
        Set(
          SimpleEdge("A", "B"),
          SimpleEdge("C", "A"),
          SimpleEdge("F", "B"),
        ),
        Set(
          SimpleEdge("D", "E"),
        )
      )
    }

    it("should perform pre-order breadth-first search") {
      val buf = mutable.ListBuffer.empty[Int]

      graph.breadthFirstSearchPreOrder(List(0), buf.addOne)

      buf.result() shouldBe List(
        0,
        1,
        5,
        6,
        4,
        9,
        10,
        11,
        12,
      )
    }
  }

}
