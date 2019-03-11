package game

import scala.collection.mutable.ArrayBuffer

sealed trait CharacterType
case object Warrior extends CharacterType
case object Monk extends CharacterType

object Direction extends Enumeration {
  val North, East, South, West = Value
}

class Character(var maxHitPoints: Int = 100, val charType: CharacterType) {
  val MaxWalkOffset = 14
  
  var hitpoints = maxHitPoints
  var walkingOffset = 0
  var maxMovementPoints = 10
  var movementPoints = maxMovementPoints
  var position = Coordinate(0, 0)
  def frame = walkingOffset / 5
  var direction = Direction.South
  var walkingPath: Option[ArrayBuffer[Coordinate]] = None
  
  def setPath(newPath: Option[ArrayBuffer[Coordinate]]): Unit = {
    walkingPath = newPath
    walkingOffset = 0
  }
  
  def clearPath(): Unit = {
    walkingPath = None
    walkingOffset = 0
  }
  
  def moveTo(newPosition: Coordinate) {
    position = newPosition
  }
  
  def restoreMovementPoints(): Unit = {
    movementPoints = maxMovementPoints
  }
  
  def walkAlongPath(): Boolean = {
    walkingPath.map(path => {
      if (walkingOffset == 0) {
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
      } else {
        walkingOffset -= 1
        false
      }
    }).getOrElse(false)
  }
  
  def isMoving: Boolean = walkingPath.isDefined
}
