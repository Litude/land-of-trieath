package game

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

class Game {
  val map = new Map(30, 30)
  val playerList: ArrayBuffer[Player] = ArrayBuffer[Player]()
  val pathFinder: PathFinder = new DjikstraFinder
  var characterIsMoving = false
  var currentPlayer = 0
  
  def moveCharacter(character: Character, destination: Coordinate) = {
    if (character.position != destination) {
      if (!characterIsMoving) {
        characterIsMoving = true
        Future {
          val currentPlayer = getCharacterPlayer(character)
          val destinationCharacter = playerList.flatMap(_.characters).find(_.position == destination)
          val filteredCharacters: Array[Character] = destinationCharacter match {
            case Some(desChar) if (currentPlayer != getCharacterPlayer(desChar)) => Array(character, desChar)
            case None => Array(character)
          }
          pathFinder.findPath(map, playerList.flatMap(_.characters).filter(!filteredCharacters.contains(_)).toArray, character.position, destination)
        }.onComplete {
          case Success(result) => {
            character.setPath(result)
            characterIsMoving = result.isDefined
          }
          case Failure(t) => characterIsMoving = false
        }
      }
    }
  }
  
  def updateGameState(): Unit = {
    if (playerList.flatMap(_.characters).filter(_.isMoving).exists(character => {
      if (character.walkingOffset == 0 && {
        character.walkingPath match {
          case Some(path) => {
            path.headOption match {
              case Some(position) => {
                if (!tileIsWalkable(position, character)) {
                  character.clearPath()
                  true
                } else {
                  false
                }
              }
              case None => false
            }
          }
          case None => false
        }
      }) {
        true
      } else {
        character.walkAlongPath()
      }
    })) characterIsMoving = false
  }
  
  def endTurn(): Unit = {
    playerList(currentPlayer).characters.foreach(_.restoreMovementPoints())
    currentPlayer += 1
    if (currentPlayer == playerList.length) currentPlayer = 0
  }
  
  def getCharacterPlayer(character: Character): Int = {
    playerList.indexOf(playerList.find(_.characters.exists(_ == character)).get)
  }
  
  def tileIsWalkable(position: Coordinate, character: Character): Boolean = {
    !map(position.x, position.y).isSolid && !playerList.flatMap(_.characters).filter(_ != character).exists(character => character.position == position)
  }
}
