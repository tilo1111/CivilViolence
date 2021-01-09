import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.language.postfixOps

object main {

  val r = scala.util.Random         //r.nextFloat

  val L: Int = 10
  var legitimacy: Double = 0.9
  var threshold: Double = 0.5
  val alfa: Int = 0               // 0 or 1
  var jailTerm: Int = 1^alfa
  var civilDens: Double = 0.5
  var copDens:Double = 0.05
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
      for (i <- 0 until L)
      {
        for (j <- 0 until L)
          print(matrix(i)(j).name + "  ")
        println()
      }
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
              matrix(x)(y) = new Civil(r.nextFloat(), L, r.nextFloat(), visionCivils)
              numOfCivils -= 1
            }
        }

      while(numOfCops > 0)
      {
        val x = r.nextInt(L)
        val y = r.nextInt(L)
        if (matrix(x)(y).name == "EMPTY")
        {
          matrix(x)(y) = new Cop(visionCops)
          numOfCops -= 1
        }
      }
    }

  def main(args:Array[String]): Unit =
  {

    var matrix = Array.ofDim[Agent](L,L)
    emptyMatrix(matrix, L)
    fillMatrix(matrix, L, civilDens, copDens)
    printMatrix(matrix, L)


  }
}