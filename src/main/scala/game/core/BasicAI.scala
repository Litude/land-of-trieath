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
            val distances = game.distancesToTargets(nextToMove.position, targets.map(_.position), characters)
            val targetIndex = distances.zipWithIndex.min._2
            if (distances(targetIndex) != Int.MaxValue) {
              game.moveCharacter(nextToMove, targets(targetIndex).position)
            } else {
              nextToMove.endTurn()
            }
          }.onComplete {
            case Success(result) => {
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
