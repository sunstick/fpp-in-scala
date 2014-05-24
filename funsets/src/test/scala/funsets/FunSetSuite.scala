package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 * - run the "test" command in the SBT console
 * - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val sBelow1000 = (elem: Int) => elem < 1000
    val sAbove500 = (elem: Int) => elem > 500
  }

  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")

      val sAll = union(sBelow1000, sAbove500)
      assert(contains(sAll, 10000), "My Union 1")
      assert(contains(sAll, 1), "My Union 2")
      assert(contains(sAll, -1000), "My Union 3")
    }
  }

  test("intersect contains intersected elements") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(contains(s, 1) == false)
      val t = intersect(sBelow1000, s1)
      for (x <- -100 to 100)
        if (x == 1)
          assert(contains(t, x))
        else
          assert(!contains(t, x))
    }
  }

  test("forall") {
    new TestSets {
      assert(forall(sAbove500, x => x > 500))
      assert(forall(sBelow1000, x => x < 1000))
      assert(forall(intersect(sAbove500, sBelow1000), x => x > 499 && x < 1001))
    }
  }

  test("exists") {
    new TestSets {
      assert(exists(sAbove500, x => x >= 1000))
      assert(exists(sBelow1000, x => x < -999))
    }
  }

  test("map negation to above 500 is below -500") {
    new TestSets {
      val m = map(sAbove500, -_)
      for (x <- -1000 to 1000)
        if (x < -500)
          assert(contains(m, x))
        else
          assert(!contains(m, x))

      assert(exists(m, _ < -700))
      assert(forall(m, _ < -400))
    }
  }
}
