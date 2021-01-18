import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.language.postfixOps
import java.io._

object main {

  val r = scala.util.Random         //r.nextFloat

  val L: Int = 40
  var legitimacy: Double = 0.82
  val alfa: Int = 1              // 0 or 1
  val T: Double = 0.1
  var JailTerm: Int = 30
  var civilDens: Double = 0.7
  var copDens:Double = 0.04
  var visionCops = 1
  var visionCivils = 1

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
            if (matrix(i)(j).name == "COP")
            {
              copsList :+= (i,j)
            }
        }
    }
    return copsList
  }
  
  def move(matrix: Array[Array[Agent]]): Unit =
  {
    for (i <- 0 until L)
    {
        for (j <- 0 until L)
        {
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
                else  matrix(i)(j).state += 1
            }
        }
    }
  }
  
  def action(matrix: Array[Array[Agent]]): Unit =
  {
    for (i <- 0 until L)
    {
        for (j <- 0 until L)
        {
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
  }
  
  def range(i: Int, j: Int, L: Int, vision: Int): List[(Int, Int)] =
  {
    val range = (0 until L).toList
    val up = if(i - vision>=0) range.slice(i-vision,i) else range.take(i) ++ range.takeRight((i - vision).abs)
    val down = if(i + vision<L) range.slice(i+1,i+1+vision) else range.take((i + vision) - L+1) ++ range.slice(i+1,L)
    val left = if(j - vision>=0) range.slice(j-vision,j) else range.take(j) ++ range.takeRight((j - vision).abs)
    val right = if(j + vision<L) range.slice(j+1,j+1+vision) else range.take((j + vision) - L+1) ++ range.slice(j+1,L)
    val vertical = (up++down).map(f => (f,j))
    val horizontal = (left++right).map(f => (i,f))
    val neighbours = vertical ++ horizontal
    return neighbours 
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
            else if (matrix(i)(j).state==(-1))  prisoners += 1
            else if (matrix(i)(j).state==1)  rebels += 1
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

    for (mcs <- 0 until 2000)
    {
      var pop = countPopulation(matrix, L)
      move(matrix)
      action(matrix)

      bw.write(mcs + " " + pop(0) + " " + pop(1) + " " + pop(2) + "\n")
    }

    //printGrievance(matrix, L)
    //printState(matrix, L)
    bw.close()

  }
}