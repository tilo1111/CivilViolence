import scala.util.Random
import scala.math._


abstract class Agent
{
  val name: String
  val vision: Int
  var state: Int
  val grievance: Double
  def move(matrix: Array[Array[Agent]], range: List[(Int, Int)], i: Int, j: Int): Unit
  def action(matrix: Array[Array[Agent]], range: List[(Int, Int)], i: Int, j: Int, T:Double): Unit
}

class Cop (v: Int) extends Agent
{
  override val vision: Int = v
  override val name: String = " COP "
  var state: Int = 2
  val grievance: Double = 0

  def move(matrix: Array[Array[Agent]], range: List[(Int, Int)], i: Int, j: Int): Unit =
  {
    //move to random position in range which is empty
    val emptys = range.filter(f => matrix(f._1)(f._2).name == "EMPTY")
    if (emptys.size != 0)
    {
      val newPos = emptys(Random.nextInt(emptys.size))
      val tmp = matrix(newPos._1)(newPos._2)
      matrix(newPos._1)(newPos._2) = matrix(i)(j)
      matrix(i)(j) = tmp
    } 
  }

  def action(matrix: Array[Array[Agent]], range: List[(Int, Int)], i: Int, j: Int, T:Double): Unit =
  {
    //do research if there are any activists arrest one randomly
    val activist = range.filter(f => matrix(f._1)(f._2).name == "CIVIL").filter(f => matrix(f._1)(f._2).state == 1)
    if(activist.size > 0)
    {
      val prisoner = activist(Random.nextInt(activist.size))
      matrix(prisoner._1)(prisoner._2).state = -1 // jail is -1
    }
  }
}

class Civil (H: Double, L: Double, R: Double, v: Int) extends Agent
{
  val hardship: Double = H
  val legitimacy: Double = L
  val riskAversion: Double = R
  override val vision: Int = v
  override val name: String = "CIVIL"

  val grievance: Double = hardship*(1-legitimacy)
  var state: Int = 0    // 0 - Quiet; 1 - Active; -1 - Jail

  def move(matrix: Array[Array[Agent]], range: List[(Int, Int)], i: Int, j: Int): Unit =
  {
    //move to random position in range which is empty
    val emptys = range.filter(f => matrix(f._1)(f._2).name == "EMPTY")
    if (emptys.size != 0)
    {
      val newPos = emptys(Random.nextInt(emptys.size))
      val tmp = matrix(newPos._1)(newPos._2)
      matrix(newPos._1)(newPos._2) = matrix(i)(j)
      matrix(i)(j) = tmp
    }  
  }

  def action(matrix: Array[Array[Agent]], range: List[(Int, Int)], i: Int, j: Int, T: Double): Unit =
  {  
    //if grievance - risk*arrest probability > T active else quiet
    //arrest probability = 1 - exponent(-k* [cops/active agents (including me)] <- in vision range)
    val k = log(10)
    val C = range.filter(f => matrix(f._1)(f._2).name == "COP").size
    val A = 1+range.filter(f => matrix(f._1)(f._2).name == "CIVIL").filter(f => matrix(f._1)(f._2).state == 1).size
    val P = 1.0 - exp(-k*(1.0*C/A))
    val condition = grievance - riskAversion*P
    matrix(i)(j).state = if(condition>T) 1 else 0  
  }
  
}

class EmpA () extends Agent
{
  var state: Int = 0
  val vision: Int = 0
  val grievance: Double = 0
  override val name: String = "EMPTY"
  def move(matrix: Array[Array[Agent]], range: List[(Int, Int)], i: Int, j: Int): Unit = {}
  def action(matrix: Array[Array[Agent]], range: List[(Int, Int)], i: Int, j: Int, T:Double): Unit = {}
}