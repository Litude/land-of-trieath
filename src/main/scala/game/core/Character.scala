package game.core

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._

class Character(val characterClass: CharacterClass) {

  def this(charType: String) {
    this(CharacterClass.get(charType))
  }

  var direction: Direction = Direction.North

  var hitpoints = maxHitPoints
  var position = Coordinate(0, 0)
  var attackTarget: Option[Character] = None

  private var _walkingOffset = 0
  private var _movementPoints = maxMovementPoints

  private var _reachableTiles = Seq[Coordinate]()
  private var _shouldUpdateReachable = true
  private var _walkingPath: Option[ArrayBuffer[Coordinate]] = None

  //This is a "VERY" lazy copy, since at least for now all other fields can be set to their defaults when copying
  def copy: Character = new Character(this.characterClass)

  def maxHitPoints: Int = characterClass.hitpoints
  def maxMovementPoints: Int = characterClass.movementPoints
  def attackPower: Int = characterClass.attackPower
  def defensePower: Int = characterClass.defensePower
  def range: Int = characterClass.range

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

  def frame: Int = if (_walkingOffset == 0) 1 else _walkingOffset / 5

  def movementPoints: Int = _movementPoints

  def walkingOffset: Int = _walkingOffset

  def clearPath(): Unit = {
    walkingPath = None
    attackTarget = None
  }

  def restoreMovementPoints(): Unit = {
    _movementPoints = maxMovementPoints
  }

  def restoreHitPoints(): Unit = {
    hitpoints = maxHitPoints
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
    _walkingOffset = Character.MaxWalkOffset
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
    val randomFactor = attackPower / Character.AttackRandomFraction
    val randomMultiplier = 2 * Random.nextDouble() - 1.0 // random value between -1.0 and 1.0

    // calculate base attack
    var finalAttackPower: Int = attackPower + (randomMultiplier * randomFactor).round.toInt

    // apply possible ranged penalty
    finalAttackPower = {
      if (this.position.tileDistance(target.position) == 1) {
        (finalAttackPower * characterClass.meleePenalty).toInt
      } else {
        finalAttackPower
      }
    }

    // apply possible attack bonus against target class
    finalAttackPower = (finalAttackPower * (characterClass.attackBonus.getOrElse(target.characterClass.toString, 1.0))).round.toInt

    // finally reduce target defense from attack
    finalAttackPower = Math.max(finalAttackPower - target.defensePower, 0)

    val originalHitpoints = target.hitpoints
    target.hitpoints = Math.max(target.hitpoints - finalAttackPower, 0)
    this._movementPoints = 0
    originalHitpoints - target.hitpoints
  }
}

object Character {

  def apply(characterClass: String): Character = new Character(characterClass)
  def apply(characterClass: CharacterClass): Character = new Character(characterClass)

  implicit val fromJson: Reads[Character] =
    (JsPath \ "class").read[String].map(Character(_))

  val AttackRandomFraction = 10.0
  val MaxWalkOffset = Tile.Size / 2 - 1
}
