package game.core

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

class BasicAI extends PlayerAI {

  val calculating = new AtomicBoolean(false)

  override def makeMove(game: Game): Boolean = {
    if (!calculating.get) {
      val movableCharacters = game.playerList(game.currentPlayer).characters.filter(_.movementPoints > 0)
      if (!movableCharacters.isEmpty) {
        val nextToMove = movableCharacters.head

        val characters = game.playerList(game.currentPlayer).characters.filter(_ != nextToMove)
        val targets = game.playerList.filter(_ != game.playerList(game.currentPlayer)).flatMap(_.characters)

        if (!targets.isEmpty) {
          calculating.set(true)
          Future {
            game.pathsToTargets(nextToMove.position, targets.map(_.position), characters)
          }.onComplete {
            case Success(result) => {
              val pathLengths = result.map(_.map(_.length).getOrElse(Int.MaxValue))
              val shortestPath = pathLengths.min
              if (shortestPath < Int.MaxValue) {
                val targetIndex = pathLengths.indexOf(shortestPath)
                nextToMove.walkingPath = result(targetIndex)
                nextToMove.attackTarget = Some(targets(targetIndex))
              } else {
                nextToMove.endTurn()
              }
              calculating.set(false)
            }
            case Failure(t) => {
              nextToMove.endTurn()
              calculating.set(false)
              throw(t)
            }
          }
        } else {
          nextToMove.endTurn()
        }
        false
      } else {
      true
      }
    } else {
      false
    }
  }
}
