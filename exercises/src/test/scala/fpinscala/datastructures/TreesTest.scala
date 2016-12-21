package fpinscala.datastructures

import org.scalatest.FunSuite

/**
  * Created by Rich on 21/12/2016.
  */
class TreesTest extends FunSuite {

  import Trees._

  val tree = Branch(
    Branch(Leaf(1), Branch(Leaf(2), Leaf(3))),
    Leaf(5)
  )

  test("size should return the number of nodes in the tree") {
    assert(4 === size(tree))
  }

  test("testMaximum") {
    assert(5 === maximum(tree))
  }

  test("depth should return the length of the tree's longest branch") {
    assert(3 === depth(tree))
  }

  test("map should apply a function to each node of the tree") {
    val treeF = Branch(
      Branch(Leaf(2), Branch(Leaf(3), Leaf(4))),
      Leaf(6)
    )
    assert(treeF === map(tree)(_ + 1))
  }

  test("fold should behave as expected for sum") {
    assert(11 === fold(tree)(_ + _))
  }

}

