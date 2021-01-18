import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.language.postfixOps
import java.io._

import scala.math.sqrt

object main {

  var s = System.currentTimeMillis
  var r = new scala.util.Random(s)

  val L: Int = 40
  var legitimacy: Double = 0.8
  val alfa: Int = 1              // 0 or 1
  val T: Double = 0.1
  var JailTerm: Int = 30
  var civilDens: Double = 0.7
  var copDens:Double = 0.04
  var visionCops = 7
  var visionCivils = 7

  def emptyMatrix(matrix: Array[Array[Agent]],  L: Int): Unit =
  {
    for (i <- 0 until L)
      for (j <- 0 until L)
      {
        matrix(i)(j) = new EmpA
      }
  }

  def printMatrix(matrix: Array[Array[Agent]],  L: Int): Unit =
  {
    for (i <- 0 until L) {
      {
        for (j <- 0 until L)
          print(matrix(i)(j).name + "  ")
        println()
      }
      println()
    }
  }

  def printGrievance(matrix: Array[Array[Agent]],  L: Int): Unit =
  {
    for (i <- 0 until L) {
      for (j <- 0 until L) {
        print((matrix(i)(j).grievance*100).toInt/100.0 + "  ")
      }
      println()
    }
    println()
  }

  def printState(matrix: Array[Array[Agent]],  L: Int): Unit =
  {
    for (i <- 0 until L) {
      for (j <- 0 until L) {
        print(matrix(i)(j).state + "  ")
      }
      println()
    }
    println()
  }

  def fillMatrix(matrix: Array[Array[Agent]],  L: Int, civilDens: Double, copDens: Double): Unit =
  {
    var numOfCops: Int = (L*L*copDens).toInt
    var numOfCivils: Int = (L*L*civilDens).toInt

    while(numOfCivils > 0)
    {
      val x = r.nextInt(L)
      val y = r.nextInt(L)
      if (matrix(x)(y).name == "EMPTY")
      {
        matrix(x)(y) = new Civil(r.nextFloat(), legitimacy, r.nextFloat(), visionCivils)
        numOfCivils -= 1
      }
    }

    while(numOfCops > 0)
    {
      val x = r.nextInt(L)
      val y = r.nextInt(L)
      if (matrix(x)(y).name == "EMPTY")
      {
        matrix(x)(y) = new Cop(visionCops, JailTerm)
        numOfCops -= 1
      }
    }
  }

  def copsPos(matrix: Array[Array[Agent]], L: Int): List[(Int, Int)] =
  {
    var copsList: List[(Int, Int)] = List()
    for (i <- 0 until L)
    {
      for (j <- 0 until L)
      {
        if (matrix(i)(j).name == " COP ")
        {
          copsList :+= (i,j)
        }
      }
    }
    return copsList
  }

  def move(matrix: Array[Array[Agent]]): Unit =
  {
    val N = (L*L*(civilDens+copDens)).toInt
    for (x <- 0 until N)
    {
      val i = r.nextInt(L)
      val j = r.nextInt(L)
      if (matrix(i)(j).name == " COP ")
      {
        matrix(i)(j).move(matrix, range(i,j,L,matrix(i)(j).vision), i, j)
      }
      else if (matrix(i)(j).name == "CIVIL")
      {
        if (matrix(i)(j).state >=0)
        {
          matrix(i)(j).move(matrix, range(i,j,L,matrix(i)(j).vision), i, j)
        }
      }
    }
  }

  def action(matrix: Array[Array[Agent]]): Unit =
  {
    val N = (L*L*(civilDens+copDens)).toInt
    for (x <- 0 until N)
    {
      val i = r.nextInt(L)
      val j = r.nextInt(L)
      if (matrix(i)(j).name == " COP ")
      {
        matrix(i)(j).action(matrix, range(i,j,L,matrix(i)(j).vision), i, j, T, JailTerm)
      }
      else if (matrix(i)(j).name == "CIVIL")
      {
        if (matrix(i)(j).state >=0)
        {
          matrix(i)(j).action(matrix, range(i,j,L,matrix(i)(j).vision), i, j, T, JailTerm)
        }
        else  matrix(i)(j).state += 1
      }
    }
  }

  def mod(x: Int, y: Int): Int =
  {
    var m: Int = x % y
    if (m < 0)
    {
      if (y < 0) m = m - y
      else m = m + y
    }
    return m
  }

  def range(i: Int, j: Int, L: Int, vision: Int): List[(Int, Int)] =
  {
    val radius = vision*vision
    var neighbours = new ListBuffer[(Int,Int)]()
    for (x <- i-vision to i+vision){
      for (y <- j-vision to j+vision){
        if(sqrt((x-i)*(x-i) + (y-j)*(y-j)) <= vision)
        {
          neighbours += Tuple2(mod(x,L),mod(y,L))
        }
      }
    }
    return neighbours.toList
  }

  def countPopulation(matrix: Array[Array[Agent]], L: Int): List[Int] =
  {
    var rebels: Int = 0
    var quiet: Int = 0
    var prisoners: Int = 0

    for (i <- 0 until L)
    {
      for (j <- 0 until L)
      {
        if (matrix(i)(j).name=="CIVIL")
        {
          if (matrix(i)(j).state==0)  quiet += 1
          else if (matrix(i)(j).state<(0))  prisoners += 1
          else rebels += 1
        }
      }
    }
    return List(rebels, quiet, prisoners)
  }

  def main(args:Array[String]): Unit =
  {
    var matrix = Array.ofDim[Agent](L,L)
    emptyMatrix(matrix, L)
    fillMatrix(matrix, L, civilDens, copDens)
    //printGrievance(matrix, L)
    //printState(matrix, L)

    val file = new File("popintime.txt")
    val bw = new BufferedWriter(new FileWriter(file))

    for (mcs <- 0 until 500)
    {
      var pop = countPopulation(matrix, L)
      move(matrix)
      action(matrix)
      //printState(matrix, L)


      bw.write(mcs + " " + pop(0) + " " + pop(1) + " " + pop(2) + "\n")
    }

    //printGrievance(matrix, L)
    //printState(matrix, L)
    bw.close()

  }
}