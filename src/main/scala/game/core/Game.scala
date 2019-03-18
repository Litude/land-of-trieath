package game.core

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import java.util.concurrent.atomic.AtomicInteger

object MovementResult extends Enumeration {
  val Failed, Moving, Attacking = Value
}

class Game(val onDamageCaused: (Int, Coordinate) => Unit) {
  val map = new Map(30, 30)
  val playerList: ArrayBuffer[Player] = ArrayBuffer[Player]()
  val pathFinder: PathFinder = new DjikstraFinder
  val multiPathFinder: MultiPathFinder = new DjikstraFinder
  val walkableTileFinder: WalkableTileFinder = new DjikstraFinder
  var characterIsMoving = false
  var currentPlayer = 0

  val pendingPathRequests = new AtomicInteger(0)

  def moveCharacter(character: Character, destination: Coordinate): MovementResult.Value = {
    val characterPlayer = getCharacterPlayer(character)
    if (characterPlayer == currentPlayer && character.position != destination && character.movementPoints > 0) {

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
        pathFinder.findPath(map, playerList.flatMap(_.characters).filter(!filteredCharacters.contains(_)), character.position, destination)
      }.onComplete {
        case Success(result) => {
          character.walkingPath = result
          characterIsMoving = characterIsMoving || result.isDefined
          character.attackTarget = targetCharacter
          pendingPathRequests.decrementAndGet()
        }
        case Failure(t) => {
          pendingPathRequests.decrementAndGet()
          throw(t)
        }
      }
      if (targetCharacter.isDefined) MovementResult.Attacking else MovementResult.Moving
    } else {
      MovementResult.Failed
    }
  }
  
  def getReachableCharacterTiles(character: Character): Seq[Coordinate] = {
    if (getCharacterPlayer(character) == currentPlayer) {
      val characters = (0 until playerList.length)
        .map(playerList)
        .flatMap(_.characters)
        .filter(_ != character)
      walkableTileFinder.findReachableTiles(map, characters, character.position, character.movementPoints)
    } else {
      Seq()
    }
  }
  
  def getPathsToTargets(start: Coordinate, targets: Seq[Coordinate], blockingCharacters: Seq[Character]) = {
    multiPathFinder.findPathsToPositions(map, blockingCharacters, start, targets)
  }
  
  def updateGameState(): Unit = {
    val movingCharacters = playerList.flatMap(_.characters).filter(_.isMoving)
    movingCharacters.foreach(updateMovingCharacter)
    removeDeadCharacters()
    characterIsMoving = !movingCharacters.isEmpty
    updateAIPlayer()
  }

  //checks if we are bumping into our attack target (and attack) or some other character (stop moving),
  //else continue walking
  def updateMovingCharacter(character: Character): Boolean = {
    if (character.movementPoints > 0 && character.atEndOfTile && {
      character.walkingPath.flatMap(_.headOption) match {
        case Some(position) => {
          character.direction = character.position.directionTo(position)
          if (character.attackTarget.map(_.position == position).getOrElse(false)) {
            character.attackTarget.foreach(target => {
              if (!target.isDead) {
                val damage = character.attackCharacter(target)
                onDamageCaused(damage, target.position)
              }
            })
            character.endTurn()
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
  
  def removeDeadCharacters(): Unit = {
    playerList.foreach(player => player.characters.find(_.isDead) match {
      case Some(deadCharacter) => player.characters -= deadCharacter
      case None =>
    })
  }

  //ends the turn and moves to the next player
  //ending a turn is disallowed as long as some character is still moving
  def endTurn(): Boolean = {
    if (!characterIsMoving && pendingPathRequests.get() == 0) {
      do {
        currentPlayer = (currentPlayer + 1) % playerList.length
      } while (!playerList(currentPlayer).isAlive)
      playerList(currentPlayer).characters.foreach(_.restoreMovementPoints())
      true
    } else {
      false
    }
    
  }
  
  //Returns player of character, or -1 if the character belongs to no player (should probably never happen)
  def getCharacterPlayer(character: Character): Int = {
    playerList.indexOf(playerList.find(_.characters.exists(_ == character)).getOrElse(ArrayBuffer[Character]()))
  }
  
  def tileIsWalkable(position: Coordinate, character: Character): Boolean = {
    !map(position.x, position.y).isSolid && !playerList.flatMap(_.characters).filter(_ != character).exists(character => character.position == position)
  }
  
  def currentPlayerType: PlayerType = playerList(currentPlayer).playerType
  
  def updateAIPlayer(): Unit = {
    playerList(currentPlayer) match {
      case player: AIPlayer => {
        if (player.ai.playTurn(this)) endTurn()
      }
      case _ =>
    }
  }
}
