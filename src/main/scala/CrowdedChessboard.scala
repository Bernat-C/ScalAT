import CrowdedChessboard.n

object CrowdedChessboard extends App{

  val e = new ScalAT("CrowdedChessboard")

  //Mida tauler
  val n = 12

  val nReines = 12;
  val nTorres = 12;
  val nAlfils = 22;
  val nCavalls = 58;

  /* objecte(i)(j) cert si la casella i j conté una peça de tipus objecte*/
  val reines: Array[Array[Int]] = e.newVar2DArray(n, n)
  val torres: Array[Array[Int]] = e.newVar2DArray(n, n)
  val alfils: Array[Array[Int]] = e.newVar2DArray(n, n)
  val cavalls: Array[Array[Int]] = e.newVar2DArray(n, n)
  val diagonals: Array[Int] = e.newVarArray(2*n-1) //Variables que indiquen si una diagonal té un alfil
  val contradiagonals: Array[Int] = e.newVarArray(2*n-1); //Variables que indiquen si una contradiagonal té un alfil

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
  for (i <- reines) {
    if(nReines >= n) e.addEOLog(i.toList)
    else e.addAMOLog(i.toList)
  }

  //A cada columna hi ha una reina
  for (i <- reines.transpose)
    if(nReines >= n) e.addEOLog(i.toList)
    else e.addAMOLog(i.toList)

  //A cada contradiagonal hi ha com a molt una reina
  for (v <- 0 to 2 * n - 2) {
    e.addAMOLog((for (i <- 0 until n; j <- 0 until n; if i + j == v) yield reines(i)(j)).toList)
  }

  //A cada diagonal hi ha com a molt una reina
  for (v <- -n + 1 until n) {
    e.addAMOLog((for (i <- 0 until n; j <- 0 until n; if i - j == v) yield reines(i)(j)).toList)
  }

  //Trencament de simetries
  //Original: Doble sat, meitat unsat
  /*
  for(i <- 0 until n )
    for(j <- i+1 until n){
      e.addClause(-reines(1)(i) :: -reines(n-1)(j) :: List())
      e.addClause(-reines(i)(1) :: -reines(j)(n-1) :: List())
    }*/
  //Lleuger augment sat, gran decrement unsat
  for (i <- 0 until n)
    for (j <- i + 1 until n) {
      e.addClause(-reines(2)(i) :: -reines(1)(j) :: List())
      e.addClause(-reines(i)(2) :: -reines(j)(1) :: List())
    }
  //Més del doble sat, menys del doble unsat
  /*
  for (i <- 0 until n)
    for (j <- i + 1 until n) {
      e.addClause(-reines(n - 1)(i) :: -reines(1)(j) :: List())
      e.addClause(-reines(i)(n - 1) :: -reines(j)(1) :: List())
    }*/


  // TORRES
  //A cada fila hi ha una torre
  for (i <- torres) {
    if(nTorres >= n) e.addEOLog(i.toList)
    else e.addAMOLog(i.toList)
  }

  //A cada columna hi ha una torre
  for (i <- torres.transpose)
    if(nTorres >= n) e.addEOLog(i.toList)
    else e.addAMOLog(i.toList)

  // ALFILS
  //A cada contradiagonal hi ha com a molt un alfil
  for (v <- 0 to 2 * n - 2) {
    e.addAMOLog((for (i <- 0 until n; j <- 0 until n; if i + j == v) yield alfils(i)(j)).toList)
  }

  //A cada diagonal hi ha com a molt un alfil
  for (v <- -n + 1 until n) {
    e.addAMOLog((for (i <- 0 until n; j <- 0 until n; if i - j == v) yield alfils(i)(j)).toList)
  }

  if(nAlfils >= 2*(n-1)) {
    //Reifiquem (????????????????????????) caselles amb alfil i diagonals
    for (v <- -n + 1 until n) {
      var casellesDiag = (for (i <- 0 until n; j <- 0 until n; if i - j == v) yield alfils(i)(j)).toList;
      for (i <- casellesDiag.indices) e.addClause(-casellesDiag(i) :: diagonals(v + n - 1) :: List())
      e.addClause(List(-diagonals(v + n - 1)).concat(casellesDiag))
    }

    //Hi ha d'haver exactament 2*(n-1) diagonals amb un alfil
    e.addEK(diagonals.toList, 2 * (n - 1))

    //Reifiquem (????????????????????????) caselles amb alfil i contradiagonals
    for (v <- -n + 1 until n) {
      var casellesContradiag = (for (i <- 0 until n; j <- 0 until n; if i - j == v) yield alfils(i)(j)).toList;
      for (i <- casellesContradiag.indices) e.addClause(-casellesContradiag(i) :: diagonals(v + n - 1) :: List())
      e.addClause(List(-contradiagonals(v + n - 1)).concat(casellesContradiag))
    }

    //Hi ha d'haver exactament 2*(n-1) diagonals amb un alfil
    e.addEK(contradiagonals.toList, 2 * (n - 1))
  }



  // CAVALLS

  //val possibleMoves = List((1, 2), (1, -2), (-1, 2), (-1, -2), (2, 1), (2, -1), (-2, 1), (-2, -1))
  val possibleMoves = List((1, -2), (-1, -2), (2, -1), (-2, -1))
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
