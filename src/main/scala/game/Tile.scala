package game

class Tile(val groundType: Int, val obstacleType: Option[Int]) {
  def isSolid: Boolean = obstacleType.isDefined
}

object Tile {
  val Size = 32
}
