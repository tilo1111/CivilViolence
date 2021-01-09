abstract class Agent
{
  val name: String
  val vision: Float
  def move(): Unit
  def action(): Unit
}

class Cop (v: Float) extends Agent
{
  override val vision: Float = v
  override val name: String = "COP"

  def move(): Unit =
  {

  }

  def action(): Unit =
  {

  }
}

class Civil (H: Double, L: Double, R: Double, v: Float) extends Agent
{
  val hardship: Double = H
  val legitimacy: Double = L
  val riskAversion: Double = R
  override val vision: Float = v
  override val name: String = "CIVIL"

  val grievance: Double = hardship*(1-legitimacy)
  val state: Int = 0    // 0 - Quiet; 1 - Active

  def move(): Unit =
  {

  }

  def action(): Unit =
  {

  }
}

class EmpA () extends Agent
{
  val vision: Float = 0
  override val name: String = "EMPTY"
  def move(): Unit = {}
  def action(): Unit = {}
}
