package game

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import java.util.concurrent.atomic.AtomicInteger

class Game(val onDamageCaused: (Int, Coordinate) => Unit) {
  val map = new Map(30, 30)
  val playerList: ArrayBuffer[Player] = ArrayBuffer[Player]()
  val pathFinder: PathFinder = new DjikstraFinder
  var characterIsMoving = false
  var currentPlayer = 0

  val pendingPathRequests = new AtomicInteger(0)

  def moveCharacter(character: Character, destination: Coordinate) = {
    if (character.position != destination) {
      val currentPlayer = getCharacterPlayer(character)

      //check if we clicked a character we can attack; if so, find the target character
      val targetCharacter = playerList.flatMap(_.characters).find(_.position == destination) match {
        case Some(desChar) if (currentPlayer != getCharacterPlayer(desChar)) => Some(desChar)
        case _ => None
      }

      //we need to remove the active and (possible) target character from the path finder so aren't considered blocking tiles
      val filteredCharacters: Array[Character] = targetCharacter match {
        case Some(desChar) => Array(character, desChar)
        case None => Array(character)
      }

      //keep count of pending requests to prevent turn from being ended while some character may still be moving
      pendingPathRequests.incrementAndGet()

      //do the path finding in a Future since the calculations can cause a few frames to get skipped
      Future {
        pathFinder.findPath(map, playerList.flatMap(_.characters).filter(!filteredCharacters.contains(_)).toArray, character.position, destination)
      }.onComplete {
        case Success(result) => {
          character.setPath(result)
          characterIsMoving = characterIsMoving || result.isDefined
          character.attackTarget = targetCharacter
          pendingPathRequests.decrementAndGet()
        }
        case Failure(t) => {
          pendingPathRequests.decrementAndGet()
          throw(t)
        }
      }
    }
  }
  
  def updateGameState(): Unit = {
    val movingCharacters = playerList.flatMap(_.characters).filter(_.isMoving)
    movingCharacters.foreach(updateMovingCharacter)
    characterIsMoving = !movingCharacters.isEmpty
  }

  //checks if we are bumping into our attack target (and attack) or some other character (stop moving),
  //else continue walking
  def updateMovingCharacter(character: Character): Boolean = {
    if (character.movementPoints > 0 && character.walkingOffset == 0 && {
      character.walkingPath.flatMap(_.headOption) match {
        case Some(position) => {
          if (character.attackTarget.map(_.position == position).getOrElse(false)) {
            character.movementPoints = 0
            character.attackTarget.foreach(target => {
              val damage = character.attackCharacter(target)
              onDamageCaused(damage, target.position)
            })
            character.clearPath()
            true
          }
          else if (!tileIsWalkable(position, character)) {
            character.clearPath()
            true
          } else {
            false
          }
        }
        case None => false
      }
    }) {
      true
    } else {
      character.walkAlongPath()
    }
  }

  //ends the turn and moves to the next player
  //ending a turn is disallowed as long as some character is still moving
  def endTurn(): Unit = {
    if (!characterIsMoving && pendingPathRequests.get() == 0) {
      playerList(currentPlayer).characters.foreach(_.restoreMovementPoints())
      currentPlayer += 1
      if (currentPlayer == playerList.length) currentPlayer = 0
    }
  }
  
  def getCharacterPlayer(character: Character): Int = {
    playerList.indexOf(playerList.find(_.characters.exists(_ == character)).get)
  }
  
  def tileIsWalkable(position: Coordinate, character: Character): Boolean = {
    !map(position.x, position.y).isSolid && !playerList.flatMap(_.characters).filter(_ != character).exists(character => character.position == position)
  }
}
