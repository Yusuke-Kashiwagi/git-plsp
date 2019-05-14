
/*
 * 作成したプログラムのアプリケーションを記述するためのファイル
 */
object Application extends App {
  val x1 = Variable("x1")
  val x2 = Variable("x2")

  val dom1 = Domain(Seq(1,2,3))
  val dom2 = Domain(Seq(1, 2, 3))

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

}
