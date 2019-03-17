package game

import scala.collection.mutable.ArrayBuffer

sealed trait CharacterType
case object Warrior extends CharacterType
case object Monk extends CharacterType

object Direction extends Enumeration {
  val North, East, South, West = Value
}

class Character(var maxHitPoints: Int = 100, val charType: CharacterType, var direction: Direction.Value) {
  val MaxWalkOffset = Tile.Size / 2 - 1
  
  val attackPower = 22
  var hitpoints = maxHitPoints
  var walkingOffset = 0
  var maxMovementPoints = 20
  var movementPoints = maxMovementPoints
  var position = Coordinate(0, 0)
  var walkingPath: Option[ArrayBuffer[Coordinate]] = None
  var attackTarget: Option[Character] = None
  
  def frame = walkingOffset / 5
  
  def setPath(newPath: Option[ArrayBuffer[Coordinate]]): Unit = {
    walkingPath = newPath
  }
  
  def clearPath(): Unit = {
    walkingPath = None
    walkingOffset = 0
    attackTarget = None
  }
  
  def moveTo(newPosition: Coordinate) {
    position = newPosition
  }
  
  def restoreMovementPoints(): Unit = {
    movementPoints = maxMovementPoints
  }
  
  def walkAlongPath(): Boolean = {
    if (walkingOffset == 0) {
      walkingPath.map(path => {
        //stop walking if at end of path or out of movement points
        if (path.length == 0 || movementPoints == 0) {
          walkingPath = None
          walkingOffset = 0
          true
        } else {
          walkingOffset = MaxWalkOffset
          direction = position.directionTo(path.head)
          position = path.head
          path.remove(0)
          walkingOffset -= 1
          movementPoints -= 1
          false
        }
      }).getOrElse(false)
    } else {
      walkingOffset -= 1
      false
    }
  }
  
  def isMoving: Boolean = walkingPath.isDefined || walkingOffset != 0
  
  def isDead: Boolean = hitpoints <= 0
  
  //attacks target character, returns the amount of damage caused
  def attackCharacter(target: Character): Int = {
    val originalHitpoints = target.hitpoints
    target.hitpoints = Math.max(target.hitpoints - attackPower, 0)
    this.movementPoints = 0
    originalHitpoints - target.hitpoints
  }
}
