package fpinscalatest.gettingstarted

import fpinscala.gettingstarted.PolymorphicFunctions
import org.scalatest.FlatSpec

/**
  * Created by davidmariassy on 08/10/17.
  */
class GettingStartedSuite extends FlatSpec {

  "PolymorphicFunctions" should "identify ordered arrays of integers" in {
    val a1 = Array[Int](1, 2, 3)
    val a2 = Array[Int](1, 2, -3)

    assert(PolymorphicFunctions.isSorted(a1, (x: Int, y: Int) => x <= y) === true)
    assert(PolymorphicFunctions.isSorted[Int](a2, (x,y) => x <= y) === false)

  }

}
