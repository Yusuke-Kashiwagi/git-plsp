
/*
 * 作成したプログラムのアプリケーションを記述するためのファイル
 */
object Application extends App {
  val x1 = Variable("x1")
  val x2 = Variable("x2")
  val x3 = Variable("x3")

  val dom1 = Domain(Seq(1,2,3))
  val dom2 = Domain(Seq(1, 2, 3))
  val dom3 = Domain(Seq(1, 4, 4))

  val c1 = Ne(x1, x2)

  val csp = CSP(Seq(x1, x2), Map(x1 -> dom1, x2 -> dom2), Seq(c1))

  csp.toSugar.foreach(println)

  def ad(xs: Seq[Variable]) ={
    for (i <- 0 until xs.size ; j <- i+1 until xs.size) yield Ne(xs(i), xs(j))
    /**
      * Alldifferent(x1, x2, x3) <==>
      *   Seq(Ne(x1, x2), Ne(x1, x3), Ne(x3, x2)
      */
  }

  val csp2 = CSP(Seq(x1, x2, x3), Map(x1 -> dom1, x2 -> dom2, x3 -> dom3), ad(Seq(x1, x2, x3)))

  //csp.toSugar.foreach(println)


}

object Test extends App{

  gt01


  def gt01 = {
    val x1 = Variable("x1")
    val x2 = Variable("x2")
    val x3 = Variable("x3")

    val vars = Seq(x1,x2,x3)

    val doms =
      Seq(
        x1 -> Domain(Seq(1, 2, 3)),
        x2 -> Domain(Seq(1, 2, 3)),
        x3 -> Domain(Seq(1, 2, 3))).toMap

    val cons =
      Seq(
        Ne(x1, x2),
        Ne(x1, x3),
        Ne(x2, x3))

    val csp = new CSP(vars, doms, cons)

    val solver = new GT

    val solution = solver.solve(csp)


    if (solution.nonEmpty) {
      println("s SAT")
      println(solution.get)
    } else {
      println("s UNSAT")
    }

  }
}
