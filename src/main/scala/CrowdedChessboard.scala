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
}
