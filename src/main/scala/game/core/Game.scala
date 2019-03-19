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
  val map = new Map(Map.TestMapSize, Map.TestMapSize)
  val playerList: ArrayBuffer[Player] = ArrayBuffer[Player]()
  val pathFinder: PathFinder = DjikstraFinder
  val multiPathFinder: MultiPathFinder = DjikstraFinder
  val walkableTileFinder: WalkableTileFinder = DjikstraFinder
  var characterIsMoving = false
  var currentPlayer = 0

  val pendingPathRequests = new AtomicInteger(0)

  def moveCharacter(character: Character, destination: Coordinate): MovementResult.Value = {
    if (isCharacterAllowedToMove(character, destination)) {

      //check if we clicked a character we can attack; if so, find the target character
      val targetCharacter = playerList
      .flatMap(_.characters)
      .find(character => character.position == destination && currentPlayer != characterPlayer(character))

      //we need to remove the active and (possible) target character from the path finder so aren't considered blocking tiles
      val filteredCharacters = Array(character) ++ targetCharacter

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
          markAllCharacterReachablesDirty()
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

  private def isCharacterAllowedToMove(character: Character, destination: Coordinate): Boolean = {
    val player = characterPlayer(character)
    actionsAllowed && player == currentPlayer && character.position != destination && character.movementPoints > 0
  }

  def updateReachableCharacterTiles(character: Character): Unit = {
    if (character.shouldUpdateReachable) {
      if (actionsAllowed && characterPlayer(character) == currentPlayer) {
        val characters = (0 until playerList.length)
          .map(playerList)
          .flatMap(_.characters)
          .filter(_ != character)
        character.reachableTiles = walkableTileFinder.findReachableTiles(map, characters, character.position, character.movementPoints)
      }
    }
  }

  def pathsToTargets(start: Coordinate, targets: Seq[Coordinate], blockingCharacters: Seq[Character]): Seq[Option[ArrayBuffer[Coordinate]]] = {
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
  //returns true if we stopped moving, false otherwise
  def updateMovingCharacter(character: Character): Boolean = {
    character.movementPoints > 0 && character.atEndOfTile match {
      case true if characterAttackedTarget(character) => true
      case true if isCharacterMovementIsObstructed(character) => true
      case _ => character.walkAlongPath()
    }
  }

  // returns true if successfully attacked target
  def characterAttackedTarget(character: Character): Boolean = {
    character.attackTarget match {
      case Some(target) if (character.position.tileDistance(target.position) <= character.range) => {
        character.direction = character.position.directionTo(target.position)
        val damage = character.attackCharacter(target)
        onDamageCaused(damage, target.position)
        character.endTurn()
        character.clearPath()
        true
      }
      case _ => false
    }
  }

  def isCharacterMovementIsObstructed(character: Character): Boolean = {
    character.walkingPath.flatMap(_.headOption) match {
      case Some(position) if (!tileIsWalkable(position, character)) => {
        character.direction = character.position.directionToAdjacent(position)
        character.clearPath()
        true
      }
      case _ => false
    }
  }

  def removeDeadCharacters(): Unit = {
    playerList.foreach(player => player.characters.find(_.isDead) match {
      case Some(deadCharacter) => player.characters -= deadCharacter
      case None =>
    })
  }

  def markAllCharacterReachablesDirty(): Unit = {
    playerList.flatMap(_.characters).foreach(_.markReachableAsDirty)
  }

  //ends the turn and moves to the next player
  //ending a turn is disallowed as long as some character is still moving
  def endTurn(): Boolean = {
    if (actionsAllowed) {
      do {
        currentPlayer = (currentPlayer + 1) % playerList.length
      } while (!playerList(currentPlayer).isAlive)
      playerList(currentPlayer).characters.foreach(_.restoreMovementPoints())
      markAllCharacterReachablesDirty()
      true
    } else {
      false
    }
  }

  def actionsAllowed: Boolean = !characterIsMoving && pendingPathRequests.get == 0

  //Returns player of character, or -1 if the character belongs to no player (should probably never happen)
  def characterPlayer(character: Character): Int = {
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
