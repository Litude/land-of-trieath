package game.core

class Tile(val groundType: Int = 0, var obstacleType: Option[Int] = None) {
  def isSolid: Boolean = obstacleType.isDefined
}

object Tile {
  val Size = 32
  val UndefinedObjectValue = 255
}
