import org.scalatest.{FlatSpec, Matchers}

import Solution.solution


class DemoTest extends FlatSpec with Matchers {
  "testCase1" should "pass" in {
    val r = solution(Array(-7,1,5,2,-4,3,0))
    r should equal( 3)
  }
  "testCase2" should "pass" in {
    val r1 = solution(Array())
    r1 should equal( -1)
  }
  "testCase3" should "pass" in {
    val r2 = solution(Array(1,0,1))
    r2 should equal( 1)
  }
  "testCase4" should "pass" in {
    val r3 = solution(Array(0,-2147483648,-2147483648))
    r3 should equal( -1)
  }
  "on one element solution" should "give solution" in {
    val r4 = solution(Array(1))
    r4 should equal( 0)
  }
}
