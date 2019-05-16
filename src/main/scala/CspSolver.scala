/*
 * CSP ソルバーに関するクラスを定義するためのファイル
 */
abstract class CspSolver {
  def solve(csp: CSP): Option[Assignment]
}

class GT extends CspSolver {
  def solve(csp: CSP): Option[Assignment] = {
    def gt(xs: Seq[Variable], partialAssign: Assignment): Option[Assignment] = {

      if (xs.isEmpty) {
        print("Generate: ")
        println(partialAssign.amap.map{case (x,v) => s"${x.name} = $v"}.mkString(", "))
        print("Test: ")
        if (csp.isSatisfiedWith(partialAssign)) {
          println("OK")
          return Some(partialAssign)
        } else {
          println("NG")
          return None
        }
      }

      val x = xs.head
      for (v <- csp.doms(x).values) {
        val sol = gt(xs.tail, partialAssign + (x -> v))
        if (sol.nonEmpty)
          return sol
      }
      None
    }

    gt(csp.vars, Assignment())
  }
}

