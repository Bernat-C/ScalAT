object CrowdedChessboard {

  val e = new ScalAT("CrowdedChessboard")

  //Mida tauler
  val n = 8

  /* tauler(i)(j) és:
  * Buit: 0
  * Reina: 1
  * Torre: 2
  * Àlfil: 3
  * Cavall: 4
   */
  val tauler: Array[Array[Int]] = e.newVar2DArray(n, n)

  // GENÈRIQUES
  //for (i <- tauler) e.addEOQuad(i.toList)

  // REINA

  // TORRE

  // ALFIL

  // CAVALL

  def getTauler = tauler
    .map(_.map((i: Int) => if (e.getValue(i)) "X " else ". "))
    .map(_.mkString(""))
    .mkString("\n")

  val result = e.solve()
  println(result)
  if (result.satisfiable) println(getTauler)
}
