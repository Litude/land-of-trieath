package game.core

import scala.collection.mutable.ArrayBuffer

sealed trait CharacterType
case object Warrior extends CharacterType
case object Monk extends CharacterType

object Direction extends Enumeration {
  val North, East, South, West = Value
}

class Character(var maxHitPoints: Int = 100, var maxMovementPoints: Int = 20, val charType: CharacterType, var direction: Direction.Value) {
  
  def this(maxHitPoints: Int, charType: CharacterType, direction: Direction.Value) {
    this(maxHitPoints = maxHitPoints, maxMovementPoints = 20, charType = charType, direction = direction)
  }
  
  val MaxWalkOffset = Tile.Size / 2 - 1
  
  private var _walkingOffset = 0
  private var _movementPoints = maxMovementPoints
  
  val attackPower = 22
  var hitpoints = maxHitPoints
  var position = Coordinate(0, 0)
  var walkingPath: Option[ArrayBuffer[Coordinate]] = None
  var attackTarget: Option[Character] = None
  
  def frame = _walkingOffset / 5
  
  def movementPoints = _movementPoints
  
  def walkingOffset = _walkingOffset
  
  def clearPath(): Unit = {
    walkingPath = None
    _walkingOffset = 0
    attackTarget = None
  }
  
  def moveTo(newPosition: Coordinate) {
    position = newPosition
  }
  
  def restoreMovementPoints(): Unit = {
    _movementPoints = maxMovementPoints
  }
  
  def walkAlongPath(): Boolean = {
    if (_walkingOffset == 0) {
      walkingPath.map(path => {
        //stop walking if at end of path or out of movement points
        if (path.length == 0 || movementPoints == 0) {
          walkingPath = None
          _walkingOffset = 0
          true
        } else {
          _walkingOffset = MaxWalkOffset
          direction = position.directionTo(path.head)
          position = path.head
          path.remove(0)
          _walkingOffset -= 1
          _movementPoints -= 1
          false
        }
      }).getOrElse(false)
    } else {
      _walkingOffset -= 1
      false
    }
  }
  
  def endTurn(): Unit = {
    _movementPoints = 0
  }
  
  def atEndOfTile: Boolean = _walkingOffset == 0
  
  def isMoving: Boolean = walkingPath.isDefined || _walkingOffset != 0
  
  def isDead: Boolean = hitpoints <= 0
  
  //attacks target character, returns the amount of damage caused
  def attackCharacter(target: Character): Int = {
    val originalHitpoints = target.hitpoints
    target.hitpoints = Math.max(target.hitpoints - attackPower, 0)
    this._movementPoints = 0
    originalHitpoints - target.hitpoints
  }
}
