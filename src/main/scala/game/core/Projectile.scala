package game.core

class Projectile(val attacker: Character, val target: Character) {

  private val direction = target.position - attacker.position
  private val xDir = direction.x / direction.vectorLength
  private val yDir = direction.y / direction.vectorLength

  private var xPos: Double = attacker.position.x * Tile.Size + Tile.Size / 4
  private var yPos: Double = attacker.position.y * Tile.Size + Tile.Size / 4

  private val initialPos = Coordinate(xPos.toInt, yPos.toInt)
  private val targetDistance = attacker.position.geometricDistance(target.position) * Tile.Size

  val angle: Int = Math.toDegrees(Math.atan2(yDir, xDir)).toInt + Projectile.ImageBaseRotation

  def update(): Unit = {
    xPos += xDir * Projectile.Velocity
    yPos += yDir * Projectile.Velocity
  }

  def position: Coordinate = Coordinate(xPos.toInt, yPos.toInt)
  def worldPosition: Coordinate = Coordinate((xPos / Tile.Size).toInt, (yPos / Tile.Size).toInt)
  def reachedTarget: Boolean = initialPos.geometricDistance(position) >= targetDistance
}

object Projectile {
  val Velocity = 4.0
  val ImageBaseRotation = 135
}
