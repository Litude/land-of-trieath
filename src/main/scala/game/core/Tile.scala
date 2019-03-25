package game.core

class Tile(val groundType: Int, var obstacleType: Option[Int]) {
  def isSolid: Boolean = obstacleType.isDefined
}

object Tile {
  val Size = 32
}
