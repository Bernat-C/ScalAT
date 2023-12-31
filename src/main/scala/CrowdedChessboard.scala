import scala.Console.{GREEN, RESET}
import scala.io.StdIn.readLine


object CrowdedChessboard extends App{

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
  var instanciaEscollida = 5 //De 0 a 11


  println("Escull la instància a resoldre (0-11):")
  instanciaEscollida = readLine().toInt
  println("Escull si el problema ha de ser satisfactible o no, afegint un cavall (0 = Insatisfactible, 1 = Satisfactible):")
  unsat = if(readLine().toInt == 0) true else false
  println("Escull l'encoding a utilitzar (0 = Quadràtic, 1 = Logarítmic):")
  configuracioLog = if(readLine().toInt == 0) false else true
  if (!unsat) {
    println("Escull si vols buscar el màxim nombre de reis que es poden col·locar al tauler (0: No, 1: Sí):")
    placeKings = if(readLine().toInt == 0) false else true
  }

  //Mida tauler
  val n = instancies(instanciaEscollida)(0)

  val nReines = instancies(instanciaEscollida)(1)
  val nTorres = instancies(instanciaEscollida)(2)
  val nAlfils = instancies(instanciaEscollida)(3)
  var nCavalls = instancies(instanciaEscollida)(4)
  if(unsat) nCavalls += 1

  /* objecte(i)(j) cert si la casella i j conté una peça de tipus objecte*/
  // Queden lligats a un ScalAT
  var reines: Array[Array[Int]] = Array.ofDim(n,n)
  var torres: Array[Array[Int]] = Array.ofDim(n,n)
  var alfils: Array[Array[Int]] = Array.ofDim(n,n)
  var cavalls: Array[Array[Int]] = Array.ofDim(n,n)
  var reis: Array[Array[Int]] = Array.ofDim(n, n)

  var diagonals: Array[Int] = Array.ofDim(2 * n - 1) //Variables que indiquen si una diagonal té un alfil
  var contradiagonals: Array[Int] = Array.ofDim(2 * n - 1); //Variables que indiquen si una contradiagonal té un alfil

  // A la posició (x)(y) hi ha cert si a la casella x y hi ha la peça corresponent
  // No estan lligats a cap ScalAT
  var reinesAux: Array[Array[Boolean]] = Array.ofDim(n,n)
  var torresAux: Array[Array[Boolean]] = Array.ofDim(n,n)
  var alfilsAux: Array[Array[Boolean]] = Array.ofDim(n,n)
  var cavallsAux: Array[Array[Boolean]] = Array.ofDim(n,n)
  var reisAux: Array[Array[Boolean]] =  Array.ofDim(n,n)


  def afegirConstraints(e: ScalAT): Unit = {
    reines = e.newVar2DArray(n, n)
    torres = e.newVar2DArray(n, n)
    alfils = e.newVar2DArray(n, n)
    cavalls = e.newVar2DArray(n, n)
    diagonals = e.newVarArray(2 * n - 1) //Variables que indiquen si una diagonal té un alfil
    contradiagonals = e.newVarArray(2 * n - 1); //Variables que indiquen si una contradiagonal té un alfil

    // GENÈRIQUES
    for (i <- 0 until n)
      for (j <- 0 until n) {
        val l = reines(i)(j) :: torres(i)(j) :: alfils(i)(j) :: cavalls(i)(j) :: List()
        if (configuracioLog) e.addAMOLog(l)
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
      if (nReines >= n) {
        if (configuracioLog) e.addEOLog(i.toList)
        else e.addEOQuad(i.toList)
      }
      else {
        if (configuracioLog) e.addAMOLog(i.toList)
        else e.addAMOQuad(i.toList)
      }
    }

    //A cada columna hi ha una reina
    for (i <- reines.transpose)
      if (nReines >= n) {
        if (configuracioLog) e.addEOLog(i.toList)
        else e.addEOQuad(i.toList)
      }
      else {
        {
          if (configuracioLog) e.addAMOLog(i.toList)
          else e.addAMOQuad(i.toList)
        }
      }

    //A cada contradiagonal hi ha com a molt una reina
    for (v <- 0 to 2 * n - 2) {
      val l = (for (i <- 0 until n; j <- 0 until n; if i + j == v) yield reines(i)(j)).toList
      if (configuracioLog) e.addAMOLog(l)
      else e.addAMOQuad(l)
    }

    //A cada diagonal hi ha com a molt una reina
    for (v <- -n + 1 until n) {
      val l = (for (i <- 0 until n; j <- 0 until n; if i - j == v) yield reines(i)(j)).toList
      if (configuracioLog) e.addAMOLog(l)
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

        if (configuracioLog) {
          //Simetria horitzontal
          e.addAMOLog(reines(1)(i) :: reines(0)(j) :: List())
          //e.addAMOLog(reines(i)(n-1) :: reines(j)(0) :: List())
          //e.addAMOLog(reines(i)(2) :: reines(j)(1) :: List())
        }
        else {
          e.addAMOQuad(reines(1)(i) :: reines(0)(j) :: List())
          //e.addAMOQuad(reines(i)(2) :: reines(j)(1) :: List())
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
      if (nTorres >= n) {
        if (configuracioLog) e.addEOLog(i.toList)
        else e.addEOQuad(i.toList)
      }
      else {
        if (configuracioLog) e.addAMOLog(i.toList)
        else e.addAMOQuad(i.toList)
      }
    }

    //A cada columna hi ha una torre
    for (i <- torres.transpose) {
      if (nTorres >= n) {
        if (configuracioLog) e.addEOLog(i.toList)
        else e.addEOQuad(i.toList)
      }
      else {
        if (configuracioLog) e.addAMOLog(i.toList)
        else e.addAMOQuad(i.toList)
      }
    }

    // ALFILS
    // Només hi ha alfils als extrems del tauler
    if(nAlfils == 2*(n-1)) {
      for (i <- 1 until n - 1) {
        for (j <- 1 until n - 1) {
          e.addClause(-alfils(i)(j) :: List())
        }
      }
    }

    //A cada contradiagonal hi ha com a molt un alfil
    for (v <- 0 to 2 * n - 2) {
      val l = (for (i <- 0 until n; j <- 0 until n; if i + j == v) yield alfils(i)(j)).toList
      if (configuracioLog) e.addAMOLog(l)
      else e.addAMOQuad(l)
    }

    //A cada diagonal hi ha com a molt un alfil
    for (v <- -n + 1 until n) {
      val l = (for (i <- 0 until n; j <- 0 until n; if i - j == v) yield alfils(i)(j)).toList
      if (configuracioLog) e.addAMOLog(l)
      else e.addAMOQuad(l)
    }

    if (nAlfils >= 2 * (n - 1)) {
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
      for (i <- possibleMoves; if i._1 + x >= 0 && i._2 + y >= 0 && i._1 + x < n && i._2 + y < n)
        if (configuracioLog) e.addAMOLog(List(cavalls(x)(y), cavalls(i._1 + x)(i._2 + y)))
        else e.addAMOQuad(List(cavalls(x)(y), cavalls(i._1 + x)(i._2 + y)))
    //e.addClause(-cavalls(x)(y) :: -cavalls(i._1+x)(i._2+y) :: List())
  }

  //val possibleMovesReis = List((1, 0), (1, -1), (0, -1), (-1, -1), (-1,0), (-1,1), (0,1), (1,1))
  val possibleMovesReis = List((1, 0), (1, -1), (0, -1), (-1, -1))
  def afegirConstraintsReis(e: ScalAT, nReis: Int): Unit = {
    afegirConstraints(e)
    reis = e.newVar2DArray(n, n)
    println("Utilitzem el constraint d'exactament " ++ nReis.toString ++ " reis.")
    e.addEK(reis.flatten.toList, nReis)
    for (i <- 0 until n)
      for (j <- 0 until n) {
        if (configuracioLog) {
          e.addAMOLog(reis(i)(j) :: reines(i)(j) :: List())
          e.addAMOLog(reis(i)(j) :: torres(i)(j) :: List())
          e.addAMOLog(reis(i)(j) :: alfils(i)(j) :: List())
          e.addAMOLog(reis(i)(j) :: cavalls(i)(j) :: List())
        }
        else {
          e.addAMOQuad(reis(i)(j) :: reines(i)(j) :: List())
          e.addAMOQuad(reis(i)(j) :: torres(i)(j) :: List())
          e.addAMOQuad(reis(i)(j) :: alfils(i)(j) :: List())
          e.addAMOQuad(reis(i)(j) :: cavalls(i)(j) :: List())
        }
      }
    for (x <- 0 until n; y <- 0 until n) {
      for (i <- possibleMovesReis; if i._1 + x >= 0 && i._2 + y >= 0 && i._1 + x < n && i._2 + y < n) {
        if (configuracioLog) e.addAMOLog(List(reis(x)(y), reis(i._1 + x)(i._2 + y)))
        else e.addAMOQuad(List(reis(x)(y), reis(i._1 + x)(i._2 + y)))
      }
    }
  }
  def getTauler = {
    for (i <- reines.indices) {
      for (j <- reines(i).indices) {
        if (reinesAux(i)(j)) {
          print("R ")
        } else if (torresAux(i)(j)) {
          print("T ")
        } else if (alfilsAux(i)(j)) {
          print("A ")
        } else if (cavallsAux(i)(j)) {
          print("C ")
        } else if (placeKings && reisAux(i)(j)) {
          print (s"${GREEN}K ${RESET}")
        } else{
          print(". ")
        }
      }
      println()
    }
  }

  def actualitzarAuxiliars(e: ScalAT, actualitzarReis: Boolean): Unit = {
    for (i <- reines.indices) {
      for (j <- reines.indices) {
        reinesAux(i)(j) = e.getValue(reines(i)(j))
        torresAux(i)(j) = e.getValue(torres(i)(j))
        alfilsAux(i)(j) = e.getValue(alfils(i)(j))
        cavallsAux(i)(j) = e.getValue(cavalls(i)(j))
        if (actualitzarReis) reisAux(i)(j) = e.getValue(reis(i)(j))
      }
    }
  }

  /*************************
   * * * * * * * * * * * * *
   * Obtenció de solucions *
   * * * * * * * * * * * * *
   *************************/

  var e = new ScalAT("CrowdedChessboard")
  var e2: ScalAT = null
  //Afegim tots els constraints a e
  afegirConstraints(e)
  val result = e.solve()
  println(result)

  //Si hem de trobar el nombre màxim de reis
  if(result.satisfiable && placeKings){
    actualitzarAuxiliars(e, false)
    val emptySpaces = n*n - nReines - nAlfils - nTorres - nCavalls
    e2 = new ScalAT("CrowdedChessboardKings")
    reis = e2.newVar2DArray(n,n)
    //Llista de caselles ocupades per altres peces
    val l = (for(i <- 0 until n; j <- 0 until n; if(
      e.getValue(reines(i)(j)) |  e.getValue(torres(i)(j)) |  e.getValue(alfils(i)(j)) |  e.getValue(cavalls(i)(j))
    )) yield reis(i)(j)).toList

    //Obtenim el nombre de reis màxim en la solució inicial
    var nReis = 0
    var result2: SolverResult = null
    var kingsTime: Double = 0
    do {
      e2 = new ScalAT("CrowdedChessboardKings")
      nReis += 1
      reis = e2.newVar2DArray(n, n)
      for (i <- l.indices) e2.addClause(-l(i) :: List())
      e2.addEK(reis.flatten.toList, nReis)
      for (x <- 0 until n; y <- 0 until n) {
        for (i <- possibleMovesReis; if i._1 + x >= 0 && i._2 + y >= 0 && i._1 + x < n && i._2 + y < n) {
          if (configuracioLog) e2.addAMOLog(List(reis(x)(y), reis(i._1 + x)(i._2 + y)))
          else e2.addAMOQuad(List(reis(x)(y), reis(i._1 + x)(i._2 + y)))
        }
      }

      result2 = e2.solve()
      kingsTime += result2.time
      if (result2.satisfiable) {
        println("* Determinat que com a mínim és satisfactible amb " ++ nReis.toString() ++ " rei(s) *")
        for (i <- reines.indices) {
          for (j <- reines.indices) {
            reisAux(i)(j) = e2.getValue(reis(i)(j))
          }
        }
      }
    } while (result2.satisfiable & nReis < emptySpaces)

    println("**********************************************************")
    if(!result2.satisfiable) {
      println("A la solució inicial hi podem posar " ++ (nReis - 1).toString ++ " rei(s).")
      println("Ara buscarem si hi ha alguna distribució que accepti més reis.")
    }
    else println("En total podem posar " ++ nReis.toString ++ " reis, ja que hem omplert tots els espais buits")
    println("King's Time: " ++ kingsTime.toString ++ "\n")

    var maximNombreTrobat = false
    while(nReis < emptySpaces && !maximNombreTrobat) {
      e = new ScalAT("CrowdedChessboard")

      println("Provem-ho amb " ++ nReis.toString ++ " reis:")
      afegirConstraintsReis(e, nReis)

      result2 = e.solve()
      kingsTime += result2.time
      println("King's time: " ++ kingsTime.toString)

      if (result2.satisfiable) {
        println("Satisfactible. Provem-ho amb un rei més.\n")
        actualitzarAuxiliars(e, true)
        nReis += 1
      }
      else {
        maximNombreTrobat = true
        println("No és satisfactible.\n")
        println("Per tant, el màxim nombre de reis que podem posar són " ++ (nReis - 1).toString ++ " rei(s):\n")
      }
    }

    getTauler
  }
    // Si no hem de trobar el nombre màxim de reis
  else if (!placeKings && result.satisfiable) {
    actualitzarAuxiliars(e, false)
    getTauler
  }
}
