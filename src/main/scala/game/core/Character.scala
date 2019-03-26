package game.core

import scala.collection.mutable.ArrayBuffer

sealed trait CharacterType {
  val range: Int = 1
}
case object Warrior extends CharacterType
case object Monk extends CharacterType
case object Ranger extends CharacterType {
  override val range = 8
}

class Character(var maxHitPoints: Int, var maxMovementPoints: Int, val charType: CharacterType, var direction: Direction) {

  val MaxWalkOffset = Tile.Size / 2 - 1

  private var _walkingOffset = 0
  private var _movementPoints = maxMovementPoints

  val attackPower = 22
  val defensePower = 1
  val range = charType.range
  var hitpoints = maxHitPoints
  var position = Coordinate(0, 0)
  var attackTarget: Option[Character] = None

  private var _reachableTiles = Seq[Coordinate]()
  private var _shouldUpdateReachable = true
  private var _walkingPath: Option[ArrayBuffer[Coordinate]] = None

  def drawingPosition: Coordinate = {
    val walkOffset = Coordinate.fromDirection(direction) * walkingOffset * 2
    val xPos = position.x * Tile.Size - walkOffset.x
    val yPos = position.y * Tile.Size - walkOffset.y
    Coordinate(xPos, yPos)
  }

  def occupiesPoint(point: Coordinate): Boolean = {
    val drawPos = drawingPosition
    point.x >= drawingPosition.x && point.x < drawingPosition.x + Tile.Size &&
    point.y >= drawingPosition.y && point.y < drawingPosition.y + Tile.Size
  }

  def walkingPath: Option[ArrayBuffer[Coordinate]] = _walkingPath
  def walkingPath_= (newPath: Option[ArrayBuffer[Coordinate]]): Unit = {
    _walkingPath = newPath
    if (_walkingPath.isDefined) markReachableAsDirty()
  }

  def shouldUpdateReachable: Boolean = _shouldUpdateReachable

  def markReachableAsDirty(): Unit = {
    _shouldUpdateReachable = true
  }

  def reachableTiles: Seq[Coordinate] = {
    if (!isMoving && ! shouldUpdateReachable) {
      _reachableTiles
    } else {
      Seq()
    }
  }

  def reachableTiles_= (newTiles: Seq[Coordinate]): Unit = {
    _reachableTiles = newTiles
    _shouldUpdateReachable = false
  }

  def frame: Int = _walkingOffset / 5

  def movementPoints: Int = _movementPoints

  def walkingOffset: Int = _walkingOffset

  def clearPath(): Unit = {
    walkingPath = None
    attackTarget = None
  }

  def restoreMovementPoints(): Unit = {
    _movementPoints = maxMovementPoints
  }

  // returns true if we stop moving, false otherwise
  def walkAlongPath(): Boolean = {
    if (atEndOfTile) {
      walkingPath match {
        case Some(path) if (path.length == 0 || movementPoints == 0) => stopMoving()
        case Some(path) => updateCharacterHeading(path)
        case _ => false
      }
    } else {
      _walkingOffset -= 1
      false
    }
  }

  private def stopMoving(): Boolean = {
    walkingPath = None
    _walkingOffset = 0
    true
  }

  private def updateCharacterHeading(path: ArrayBuffer[Coordinate]): Boolean = {
    direction = position.directionTo(path.head)
    position = path.head
    path.remove(0)
    _walkingOffset = MaxWalkOffset
    _walkingOffset -= 1
    _movementPoints -= 1
    false
  }

  def endTurn(): Unit = {
    _shouldUpdateReachable = true
    _movementPoints = 0
  }

  def atEndOfTile: Boolean = _walkingOffset == 0

  def isMoving: Boolean = walkingPath.isDefined || _walkingOffset != 0

  def isDead: Boolean = hitpoints <= 0

  def isRangedFighter: Boolean = range > 1

  //attacks target character, returns the amount of damage caused
  def attackCharacter(target: Character): Int = {
    val originalHitpoints = target.hitpoints
    target.hitpoints = Math.max(target.hitpoints - attackPower, 0)
    this._movementPoints = 0
    originalHitpoints - target.hitpoints
  }
}
