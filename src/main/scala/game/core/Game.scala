package game.core

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object MovementResult extends Enumeration {
  val Failed, Moving, Attacking = Value
}

class Game(
    val playerList: Seq[Player],
    val map: Map,
    var onDamageCaused: (Int, Coordinate) => Unit = (Int, Coordinate) => Unit,
    var onTurnEnded: () => Unit = () => Unit
  ) {
  //val map = MapGenerator.generateMap(Map.TestMapSize, Map.TestMapSize, playerList.length)
  val pathFinder: PathFinder = DjikstraFinder
  val walkableTileFinder: WalkableTileFinder = DjikstraFinder
  var characterIsMoving = false
  var isPaused = false
  var currentPlayer = 0
  var projectiles = ArrayBuffer[Projectile]()

  val pendingPathRequests = new AtomicInteger(0)

  //place player characters to spawns
  playerList.zip(map.spawns).foreach({case (player, spawnList) => {
    player.characters.zip(spawnList).foreach({case (character, spawn) => {
      character.position = spawn.position
      character.direction = spawn.direction
    }})
  }})

  def moveCharacter(character: Character, destination: Coordinate): MovementResult.Value = {
    if (isCharacterAllowedToMove(character, destination)) {

      //check if we clicked a character we can attack; if so, find the target character
      val targetCharacter = playerList
      .flatMap(_.characters)
      .find(character => character.position == destination && currentPlayer != characterPlayer(character))

      //keep count of pending requests to prevent turn from being ended while some character may still be moving
      pendingPathRequests.incrementAndGet()

      //do the path finding in a Future since the calculations can cause a few frames to get skipped
      Future {
        targetCharacter match {
          case Some(target) => {
            pathFinder.findPathToTarget(map, playerList.flatMap(_.characters).filter(_ != character), character.position, target.position, character.range)
          }
          case _ => pathFinder.findPath(map, playerList.flatMap(_.characters).filter(_ != character), character.position, destination)
        }
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

  def updateReachableCharacterTiles(character: Character): Unit = {
    if (character.shouldUpdateReachable) {
      val ownerPlayer = characterPlayer(character)
      if (actionsAllowed && ownerPlayer == currentPlayer) {
        val friendlyCharacters = playerList(ownerPlayer).characters.filter(_ != character)
        val enemyCharacters = (0 until playerList.length)
          .filter(_ != ownerPlayer)
          .map(playerList)
          .flatMap(_.characters)
        character.reachableTiles = walkableTileFinder.findReachableTiles(map, friendlyCharacters, enemyCharacters, character.position, character.movementPoints)
      }
    }
  }

  def distancesToTargets(start: Coordinate, targets: Seq[Coordinate], blockingCharacters: Seq[Character]): Seq[Int] = {
    pathFinder.findDistancesToPositions(map, blockingCharacters, start, targets)
  }

  def updateGameState(): Unit = {
    if (!isPaused) {
      val movingCharacters = playerList.flatMap(_.characters).filter(_.isMoving)
      movingCharacters.foreach(updateMovingCharacter)
      updateProjectiles()
      removeDeadCharacters()
      characterIsMoving = !movingCharacters.isEmpty
      updateAIPlayer()
    }
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
      onTurnEnded()
      true
    } else {
      false
    }
  }

  def actionsAllowed: Boolean = !isPaused && !characterIsMoving && pendingPathRequests.get == 0 && projectiles.length == 0

  //Returns player of character, or -1 if the character belongs to no player (should probably never happen)
  def characterPlayer(character: Character): Int = {
    playerList.indexOf(playerList.find(_.characters.exists(_ == character)).getOrElse(ArrayBuffer[Character]()))
  }

  def currentPlayerType: PlayerType = playerList(currentPlayer).playerType

  def isOver: Option[Int] = {
    val playersAlive = playerList.filter(_.isAlive)
    if (playersAlive.length == 1) Some(playerList.indexOf(playersAlive.head)) else None
  }

  private def tileIsWalkable(position: Coordinate, character: Character): Boolean = {
    !map(position.x, position.y).isSolid && !playerList.flatMap(_.characters).filter(_ != character).exists(character => character.position == position)
  }

  private def isCharacterAllowedToMove(character: Character, destination: Coordinate): Boolean = {
    val player = characterPlayer(character)
    actionsAllowed && player == currentPlayer && character.position != destination && character.movementPoints > 0
  }

  //checks if we are bumping into our attack target (and attack) or some other character (stop moving),
  //else continue walking
  //returns true if we stopped moving, false otherwise
  private def updateMovingCharacter(character: Character): Boolean = {
    character.movementPoints > 0 && character.atEndOfTile match {
      case true if characterAttackedTarget(character) => true
      case true if isCharacterMovementIsObstructed(character) => true
      case _ => character.walkAlongPath()
    }
  }

  // returns true if successfully attacked target
  private def characterAttackedTarget(character: Character): Boolean = {
    character.attackTarget match {
      case Some(target) if (character.position.tileDistance(target.position) <= character.range) => {
        character.direction = character.position.directionTo(target.position)
        if (character.isRangedFighter && character.position.tileDistance(target.position) > 1) {
          projectiles += new Projectile(character, target)
        } else {
          characterAttackTarget(character, target)
        }
        character.endTurn()
        character.clearPath()
        true
      }
      case _ => false
    }
  }

  private def characterAttackTarget(attacker: Character, target: Character): Unit = {
    val damage = attacker.attackCharacter(target)
    onDamageCaused(damage, target.position)
  }

  private def isCharacterMovementIsObstructed(character: Character): Boolean = {
    character.walkingPath.flatMap(_.headOption) match {
      case Some(position) if (!tileIsWalkable(position, character)) => {
        character.direction = character.position.directionTo(position)
        character.clearPath()
        true
      }
      case _ => false
    }
  }

  private def removeDeadCharacters(): Unit = {
    playerList.foreach(player => player.characters.find(_.isDead) match {
      case Some(deadCharacter) => player.characters -= deadCharacter
      case None =>
    })
  }

  private def updateProjectiles(): Unit = {
    projectiles.foreach(_.update())
    projectiles.filter(_.reachedTarget).foreach(projectile => {
      characterAttackTarget(projectile.attacker, projectile.target)
    })
    projectiles = projectiles.filter(!_.reachedTarget)
  }

  private def markAllCharacterReachablesDirty(): Unit = {
    playerList.flatMap(_.characters).foreach(_.markReachableAsDirty)
  }

  private def updateAIPlayer(): Unit = {
    playerList(currentPlayer) match {
      case player: AIPlayer => {
        if (player.ai.playTurn(this)) endTurn()
      }
      case _ =>
    }
  }
}
