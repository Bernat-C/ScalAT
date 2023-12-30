import CrowdedChessboard.{n, possibleMoves}

object CrowdedChessboard extends App{

  val e = new ScalAT("CrowdedChessboard")
  var e2 = new ScalAT("CrowdedChessboardKings")

  val instancies: Array[Array[Int]] = Array(
    Array(5,5,5,8,5),
    Array(6,6,6,10,9),
    Array(7,7,7,12,15),
    Array(8,8,8,14,21),
    Array(9,9,9,16,29),
    Array(10,10,10,18,37),
    Array(11,11,11,20,47),
    Array(12,12,12,22,57),
    Array(13,13,13,24,69),
    Array(14,14,14,26,81),
    Array(15,15,15,28,94),
    Array(16,16,16,30,109),
  )

  var configuracioLog = true;
  var unsat = false;
  var placeKings = true;
  var instanciaEscollida = 4 //De 0 a 11
  //Mida tauler
  val n = instancies(instanciaEscollida)(0)

  val nReines = instancies(instanciaEscollida)(1)
  val nTorres = instancies(instanciaEscollida)(2)
  val nAlfils = instancies(instanciaEscollida)(3)
  var nCavalls = instancies(instanciaEscollida)(4)
  if(unsat) nCavalls += 1

  /* objecte(i)(j) cert si la casella i j conté una peça de tipus objecte*/
  val reines: Array[Array[Int]] = e.newVar2DArray(n, n)
  val torres: Array[Array[Int]] = e.newVar2DArray(n, n)
  val alfils: Array[Array[Int]] = e.newVar2DArray(n, n)
  val cavalls: Array[Array[Int]] = e.newVar2DArray(n, n)
  val diagonals: Array[Int] = e.newVarArray(2*n-1) //Variables que indiquen si una diagonal té un alfil
  val contradiagonals: Array[Int] = e.newVarArray(2*n-1); //Variables que indiquen si una contradiagonal té un alfil

  var reis: Array[Array[Int]] = e2.newVar2DArray(n, n)
  var reisAux: Array[Array[Boolean]] =  Array.ofDim(n,n)
  var reisCopy: Array[Array[Boolean]] = reisAux.map(_.clone())


  // GENÈRIQUES
  for (i <- 0 until n)
    for (j <- 0 until n) {
      val l = reines(i)(j) :: torres(i)(j) :: alfils(i)(j) :: cavalls(i)(j) :: List()
      if(configuracioLog) e.addAMOLog(l)
      else e.addAMOQuad(l)
    }

  // CONSTRAINTS SEGONS ENUNCIAT
  e.addEK(reines.flatten.toList, nReines)
  e.addEK(torres.flatten.toList, nTorres)
  e.addEK(alfils.flatten.toList, nAlfils)
  //e.addEK((for(i <- 0 until n; j <- 0 until n; if(i == 0 | j == 0 | i == n-1 | j == n-1)) yield alfils(i)(j)).toList, nAlfils)
  e.addEK(cavalls.flatten.toList, nCavalls)

  // REINES
  //A cada fila hi ha una reina
  for (i <- reines) {
    if(nReines >= n) {
      if(configuracioLog) e.addEOLog(i.toList)
      else e.addEOQuad(i.toList)
    }
    else {
      if(configuracioLog) e.addAMOLog(i.toList)
      else e.addAMOQuad(i.toList)
    }
  }

  //A cada columna hi ha una reina
  for (i <- reines.transpose)
    if(nReines >= n) {
      if(configuracioLog) e.addEOLog(i.toList)
      else e.addEOQuad(i.toList)
    }
    else {
      {
        if(configuracioLog) e.addAMOLog(i.toList)
        else e.addAMOQuad(i.toList)
      }
    }

  //A cada contradiagonal hi ha com a molt una reina
  for (v <- 0 to 2 * n - 2) {
    val l = (for (i <- 0 until n; j <- 0 until n; if i + j == v) yield reines(i)(j)).toList
    if(configuracioLog) e.addAMOLog(l)
    else e.addAMOQuad(l)
  }

  //A cada diagonal hi ha com a molt una reina
  for (v <- -n + 1 until n) {
    val l = (for (i <- 0 until n; j <- 0 until n; if i - j == v) yield reines(i)(j)).toList
    if(configuracioLog) e.addAMOLog(l)
    else e.addAMOQuad(l)
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
      //e.addClause(-reines(2)(i) :: -reines(1)(j) :: List())
      //e.addClause(-reines(i)(2) :: -reines(j)(1) :: List())

      if(configuracioLog) {
        e.addAMOLog(reines(2)(i) :: reines(1)(j) :: List())
        e.addAMOLog(reines(i)(2) :: reines(j)(1) :: List())
      }
      else {
        e.addAMOQuad(reines(2)(i) :: reines(1)(j) :: List())
        e.addAMOQuad(reines(i)(2) :: reines(j)(1) :: List())
      }
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
    if(nTorres >= n) {
      if(configuracioLog) e.addEOLog(i.toList)
      else e.addEOQuad(i.toList)
    }
    else {
      if(configuracioLog) e.addAMOLog(i.toList)
      else e.addAMOQuad(i.toList)
    }
  }

  //A cada columna hi ha una torre
  for (i <- torres.transpose) {
    if(nTorres >= n) {
      if(configuracioLog) e.addEOLog(i.toList)
      else e.addEOQuad(i.toList)
    }
    else {
      if(configuracioLog) e.addAMOLog(i.toList)
      else e.addAMOQuad(i.toList)
    }
  }

  // ALFILS
  // Només hi ha alfils als extrems del tauler
  for(i <- 1 until n-1){
    for(j <- 1 until n-1){
      e.addClause(-alfils(i)(j) :: List())
    }
  }

  //e.addEOLog(alfils(0)(0) :: alfils(n-1)(n-1) :: List())
  //e.addEOLog(alfils(0)(n-1) :: alfils(n-1)(0) :: List())

  //A cada contradiagonal hi ha com a molt un alfil
  for (v <- 0 to 2 * n - 2) {
    val l = (for (i <- 0 until n; j <- 0 until n; if i + j == v) yield alfils(i)(j)).toList
    if(configuracioLog) e.addAMOLog(l)
    else e.addAMOQuad(l)
  }

  //A cada diagonal hi ha com a molt un alfil
  for (v <- -n + 1 until n) {
    val l = (for (i <- 0 until n; j <- 0 until n; if i - j == v) yield alfils(i)(j)).toList
    if(configuracioLog) e.addAMOLog(l)
    else e.addAMOQuad(l)
  }

  if(nAlfils >= 2*(n-1)) {
    //Reifiquem (????????????????????????) caselles amb alfil i diagonals
    for (v <- -n + 1 until n) {
      var casellesDiag = (for (i <- 0 until n; j <- 0 until n; if i - j == v) yield alfils(i)(j)).toList;
      for (i <- casellesDiag.indices) e.addClause(-casellesDiag(i) :: diagonals(v + n - 1) :: List())
      //e.addClause(List(-diagonals(v + n - 1)).concat(casellesDiag))
      e.addALO(List(-diagonals(v + n - 1)).concat(casellesDiag))
    }

    //Hi ha d'haver exactament 2*(n-1) diagonals amb un alfil
    e.addEK(diagonals.toList, 2 * (n - 1))

    //Reifiquem (????????????????????????) caselles amb alfil i contradiagonals
    //Amb això va més ràpid per alguna raó
    /*for (v <- -n + 1 until n) {
      var casellesContradiag = (for (i <- 0 until n; j <- 0 until n; if i - j == v) yield alfils(i)(j)).toList;
      for (i <- casellesContradiag.indices) e.addClause(-casellesContradiag(i) :: diagonals(v + n - 1) :: List())
      //e.addClause(List(-contradiagonals(v + n - 1)).concat(casellesContradiag))
      e.addALO(List(-contradiagonals(v + n - 1)).concat(casellesContradiag))
    }*/

    for (v <- 0 to 2 * n - 2) {
      var casellesContradiag = (for (i <- 0 until n; j <- 0 until n; if i + j == v) yield alfils(i)(j)).toList;
      for (i <- casellesContradiag.indices) e.addClause(-casellesContradiag(i) :: contradiagonals(v) :: List())
      //e.addClause(List(-contradiagonals(v + n - 1)).concat(casellesContradiag))
      e.addALO(List(-contradiagonals(v)).concat(casellesContradiag))
    }

    //Hi ha d'haver exactament 2*(n-1) diagonals amb un alfil
    e.addEK(contradiagonals.toList, 2 * (n - 1))
  }



  // CAVALLS

  //val possibleMoves = List((1, 2), (1, -2), (-1, 2), (-1, -2), (2, 1), (2, -1), (-2, 1), (-2, -1))
  val possibleMoves = List((1, -2), (-1, -2), (2, -1), (-2, -1))
  for (x <- 0 until n; y <- 0 until n)
    for (i <- possibleMoves; if i._1+x >= 0 && i._2+y >= 0 && i._1+x < n && i._2+y < n)
      if(configuracioLog) e.addAMOLog(List(cavalls(x)(y),cavalls(i._1+x)(i._2+y)))
      else e.addAMOQuad(List(cavalls(x)(y),cavalls(i._1+x)(i._2+y)))
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
        } else if (placeKings && reisAux(i)(j)) {
          print ("K ")
        } else{
          print(". ")
        }
      }
      println()
    }
  }

  def getTaulerDummy = {
    for (i <- reines.indices) {
      for (j <- reines(i).indices) {
        if (placeKings && e2.getValue(reis(i)(j))) print ("K ")
        else print (". ")
      }
      println()
    }
  }

  val result = e.solve()
  println(result)

  //Reis
  if(result.satisfiable && placeKings){
    val emptySpaces = n*n - nReines - nAlfils - nTorres - nCavalls
    val possibleMoves = List((1, 0), (1, -1), (0, -1), (-1, -1))

    val l = (for(i <- 0 until n; j <- 0 until n; if(
      e.getValue(reines(i)(j)) |  e.getValue(torres(i)(j)) |  e.getValue(alfils(i)(j)) |  e.getValue(cavalls(i)(j))
    )) yield reis(i)(j)).toList

    /*for(x <- 0 until n; y <- 0 until n){
      for (i <- possibleMoves; if i._1 + x >= 0 && i._2 + y >= 0 && i._1 + x < n && i._2 + y < n) {
        if (configuracioLog) e.addAMOLog(List(reis(x)(y), reis(i._1 + x)(i._2 + y)))
        else e.addAMOQuad(List(reis(x)(y), reis(i._1 + x)(i._2 + y)))
      }
    }*/
    /*
    for (i <- 0 until n ; j <- 0 until n; if (
      e.getValue(reines(i)(j)) | e.getValue(torres(i)(j)) | e.getValue(alfils(i)(j)) | e.getValue(cavalls(i)(j))
      )) e2.addClause(-reis(i)(j) :: List())*/

    for (i <- l.indices) e2.addClause(-l(i) :: List())
    var nReis = 1
    e2.addEK(reis.flatten.toList, nReis)
    var result2 = e2.solve()
    while(result2.satisfiable & nReis < emptySpaces) {
      nReis += 1
      e2 = new ScalAT("CrowdedChessboardKings")
      reis = e2.newVar2DArray(n, n)
      for (i <- l.indices) e2.addClause(-l(i) :: List())
      e2.addEK(reis.flatten.toList, nReis)

      result2 = e2.solve()
      if(result2.satisfiable) {
        for(i <- reines.indices){
          for(j <- reines.indices){
            reisAux(i)(j) = e2.getValue(reis(i)(j))
          }
        }
      }
    }

    //nReis += 1
    //e2.addEK(reis.flatten.toList, nReis)
    //var result2 = e2.solve()
    /*
    while(result2.satisfiable && nReis < 3){
      nReis += 1
      e2 = new ScalAT("CrowdedChessboardKings")
      reis = e2.newVar2DArray(n, n)
      for (i <- l.indices) e2.addClause(-l(i) :: List())
      e2.addEK(reis.flatten.toList, nReis)
      result2 = e2.solve()
    }*/
    //println("En total podem posar " ++ (nReis - 1).toString ++ " Reis")
    if(result2.satisfiable) println(getTauler)
  }

  if (!placeKings && result.satisfiable) println(getTauler)
}
