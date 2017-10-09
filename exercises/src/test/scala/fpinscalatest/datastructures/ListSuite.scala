package fpinscalatest.datastructures

import org.scalatest._
import fpinscala.datastructures.List

/**
  * Created by davidmariassy on 08/10/17.
  */
class ListSuite extends FlatSpec {

  val testList = List(1,2,3)

  "List" should "return its tail" in {

    assert(List.tail(testList) === List(2,3))
    assert(List.tail(List()) === List())
  }

  it should "replace its head" in {
    assert(List.setHead(testList, 3) === List(3,2,3))
    assert(List.setHead(List(1), 3) === List(3))
    assert(List.setHead(List(), 1) === List())
  }

  it should "drop n elements" in {
    assert(List.drop(testList, 2) === List(3))
  }

  it should "drop while the condition is true" in {
    assert(List.dropWhile(List(1,2,3,4,5), (x: Int) => x < 3) === List(3,4,5))
  }

  it should "return all but last element of object" in {
    assert(List.init(testList) === List(1,2))
    assert(List.init2(testList) === List(1,2))
  }

  it should "calculate the length of lists" in {
    assert(List.length(testList) === 3)
  }

}
