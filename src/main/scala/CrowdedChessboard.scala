object CrowdedChessboard extends App{

  val e = new ScalAT("CrowdedChessboard")

  //Mida tauler
  val n = 9

  val nReines = 9;
  val nTorres = 9;
  val nAlfils = 16;
  val nCavalls = 29;

  /* objecte(i)(j) cert si la casella i j conté una peça de tipus objecte*/
  val reines: Array[Array[Int]] = e.newVar2DArray(n, n)
  val torres: Array[Array[Int]] = e.newVar2DArray(n, n)
  val alfils: Array[Array[Int]] = e.newVar2DArray(n, n)
  val cavalls: Array[Array[Int]] = e.newVar2DArray(n, n)

  // GENÈRIQUES
  for (i <- 0 until n)
    for (j <- 0 until n)
      e.addAMOLog(reines(i)(j) :: torres(i)(j) :: alfils(i)(j) :: cavalls(i)(j) :: List())

  // CONSTRAINTS SEGONS ENUNCIAT
  e.addEK(reines.flatten.toList, nReines)
  e.addEK(torres.flatten.toList, nTorres)
  e.addEK(alfils.flatten.toList, nAlfils)
  e.addEK(cavalls.flatten.toList, nCavalls)

  // REINES
  //A cada fila hi ha una reina
  for (i <- reines)
    e.addAMOLog(i.toList)

  //A cada columna hi ha una reina
  for (i <- reines.transpose)
    e.addAMOLog(i.toList)

  //A cada contradiagonal hi ha com a molt una reina
  for (v <- 0 to 2 * n - 2) {
    e.addAMOLog((for (i <- 0 until n; j <- 0 until n; if i + j == v) yield reines(i)(j)).toList)
  }

  //A cada diagonal hi ha com a molt una reina
  for (v <- -n + 1 until n) {
    e.addAMOLog((for (i <- 0 until n; j <- 0 until n; if i - j == v) yield reines(i)(j)).toList)
  }

  // TORRES
  //A cada fila hi ha una torre
  for (i <- torres) e.addAMOLog(i.toList)

  //A cada columna hi ha una torre
  for (i <- torres.transpose) e.addAMOLog(i.toList)

  // ALFILS
  //A cada contradiagonal hi ha com a molt un alfil
  for (v <- 0 to 2 * n - 2) {
    e.addAMOLog((for (i <- 0 until n; j <- 0 until n; if i + j == v) yield alfils(i)(j)).toList)
  }

  //A cada diagonal hi ha com a molt un alfil
  for (v <- -n + 1 until n) {
    e.addAMOLog((for (i <- 0 until n; j <- 0 until n; if i - j == v) yield alfils(i)(j)).toList)
  }

  // CAVALLS
  val possibleMoves = List((1, 2), (1, -2), (-1, 2), (-1, -2), (2, 1), (2, -1), (-2, 1), (-2, -1))

  for (x <- 0 until n; y <- 0 until n)
    for (i <- possibleMoves; if i._1+x >= 0 && i._2+y >= 0 && i._1+x < n && i._2+y < n)
      e.addAMOLog(List(cavalls(x)(y),cavalls(i._1+x)(i._2+y)))
      //e.addClause(-cavalls(x)(y) :: -cavalls(i._1+x)(i._2+y) :: List())

  def getTauler = {
    for (i <- reines.indices) {
      for (j <- reines(i).indices) {
        if (e.getValue(reines(i)(j))) {
          print("R ")
        } else if (e.getValue(torres(i)(j))) {
          print("T ")
        } else if (e.getValue(alfils(i)(j))) {
          print("A ")
        } else if (e.getValue(cavalls(i)(j))) {
          print("C ")
        } else {
          print(". ")
        }
      }
      println()
    }
  }

  val result = e.solve()
  println(result)
  if (result.satisfiable) println(getTauler)
}
