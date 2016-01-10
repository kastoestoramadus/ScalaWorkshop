
object TestApp extends App {
  import Solution.solution
  var r = solution(Array(-7,1,5,2,-4,3,0))
  assert ( r == 3, r)
  val r1 = solution(Array())
  assert ( r1 == -1, r1)
  val r2 = solution(Array(1,0,1))
  assert ( r2 == 1, r2)
  val r3 = solution(Array(0,-2147483648,-2147483648))
  assert ( r3 == -1, r3)
  println("Success")
}
