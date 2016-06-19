package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


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
   *   val s1 = singletonSet(1)
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

    val even = (i: Int) => i % 2 == 0;
    val odd = (i: Int) => i % 2 != 0;
    val negative = (i: Int) => i < 0;
    val positive = (i: Int) => i > 0;

    val identity = (i: Int) => i
    val one_more = (i: Int) => i + 1
    val double = (i: Int) => 2 * i
    val triple = (i: Int) => 3 * i
    val negate = (i: Int) => -1 * i
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Does not contain 2")
      assert(!contains(s1, 3), "Does not contain 3")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect 1, 2 does not contain 1")
      assert(!contains(s, 2), "Intersect 1, 2 does not contain 2")

      val t = intersect(union(s1, s2), union(s1, s3))
      assert(contains(t, 1), "Intersection contains 1")
      assert(!contains(t, 2), "Intersection does not contain 2")
      assert(!contains(t, 3), "Intersection does not contain 3")
    }
  }

  test("diff") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff 1, 2 does not contain 1")
      assert(!contains(s, 2), "Diff 1, 2 does not contain 2")

      val t = diff(union(s1, s2), union(s1, s3))
      assert(!contains(t, 1), "Diff does not contain 1")
      assert(contains(t, 2), "Diff contains 2")
      assert(!contains(t, 3), "Diff does not contain 3")

      val u = diff(s1, union(s2, s3))
      assert(contains(u, 1), "Diff contains 1")
      assert(!contains(u, 2), "Diff does not contain 2")
      assert(!contains(u, 3), "Diff does not contain 3")

      val v = diff(union(s2, s3), s1)
      assert(!contains(v, 1), "Diff does not contain 1")
      assert(contains(v, 2), "Diff contains 2")
      assert(contains(v, 3), "Diff contains 3")
    }
  }

  test("filter") {
    new TestSets {
      assert(contains(filter(s1, odd), 1), "1 is odd")
      assert(!contains(filter(s1, even), 1), "1 is not even")

      assert(contains(filter(s2, even), 2), "2 is odd")
      assert(!contains(filter(s2, odd), 1), "2 is not odd")

      val s = union(s1, union(s2, s3))
      assert(!contains(filter(s, negative), 1), "1 is not negative")
      assert(!contains(filter(s, negative), 2), "2 is not negative")
      assert(!contains(filter(s, negative), 3), "3 is not negative")
    }
  }

  test("forall") {
    new TestSets {
      assert(forall(s1, odd), "1 is odd")
      assert(!forall(s1, even), "1 is not even")

      assert(forall(s2, even), "2 is even")
      assert(!forall(s2, odd), "2 is not odd")

      val s = union(s1, s3)
      assert(forall(s, odd), "1 and 3 are odd")
      assert(!forall(s, even), "1 nor 3 is even")

      val t = union(s1, s2)
      assert(!forall(t, odd), "not all odd")
      assert(!forall(t, even), "not all even")
      assert(forall(t, positive), "all positive")
      assert(!forall(t, negative), "not all negative")
    }
  }

  test("exists") {
    new TestSets {
      assert(exists(s1, odd), "1 is odd")
      assert(!exists(s1, even), "1 is not even")

      assert(exists(s2, even), "2 is even")
      assert(!exists(s2, odd), "2 is not odd")

      val s = union(s1, s3)
      assert(exists(s, odd), "1 and 3 are odd")
      assert(!exists(s, even), "1 nor 3 is even")

      val t = union(s1, s2)
      assert(exists(t, odd), "1 is odd")
      assert(exists(t, even), "2 is even")
      assert(exists(t, positive), "all positive")
      assert(!exists(t, negative), "not all negative")
    }
  }

  test("map") {
    new TestSets {
      assert(contains(map(s1, identity), 1), "1")
      assert(contains(map(s1, one_more), 2), "1 + 1 = 2")
      assert(contains(map(s1, double), 2), "1 * 2 = 2")
      assert(contains(map(s1, triple), 3), "1 * 3 = 3")
      assert(contains(map(s1, negate), -1), "1 * -1 = -1")

      val s = union(s1, s2)
      assert(contains(map(s, identity), 1), "1")
      assert(contains(map(s, identity), 2), "2")
      assert(!contains(map(s, identity), 3), "not 3")
      assert(contains(map(s, one_more), 2), "1 + 1 = 2")
      assert(contains(map(s, one_more), 3), "2 + 1 = 3")
      assert(!contains(map(s, one_more), 4), "no 3")
      assert(contains(map(s, double), 2), "1 * 2 = 2")
      assert(contains(map(s, double), 4), "2 * 2 = 4")
      assert(!contains(map(s, double), 6), "no 3")
      assert(contains(map(s, triple), 3), "1 * 3 = 3")
      assert(contains(map(s, triple), 6), "2 * 3 = 6")
      assert(!contains(map(s, triple), 9), "no 3")
      assert(contains(map(s, negate), -1), "1 * -1 = -1")
      assert(contains(map(s, negate), -2), "2 * -1 = -2")
      assert(!contains(map(s, negate), -3), "no 3")
    }
  }
}
