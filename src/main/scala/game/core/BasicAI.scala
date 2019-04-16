package game.core

import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class BasicAI extends PlayerAI {

  val calculating = new AtomicBoolean(false)

  override def makeMove(game: Game): Boolean = {
    val movableCharacters = game.playerList(game.currentPlayer).characters.filter(_.movementPoints > 0)
    if (!movableCharacters.isEmpty) {
      val nextToMove = movableCharacters.head

      val characters = game.playerList(game.currentPlayer).characters.filter(_ != nextToMove)
      val targets = game.playerList.filter(_ != game.playerList(game.currentPlayer)).flatMap(_.characters)

      if (!targets.isEmpty) {
        calculating.set(true)
        Future {
          // priority here is an inverted priority, i.e. a target with a lower value is more likely to be attacked
          var priorities = game.distancesToTargets(nextToMove.position, targets.map(_.position), characters).zipWithIndex.filter(_._1 != Int.MaxValue)
          priorities = priorities.map({ case (distance: Int, i: Int) => {
            // equalize all targets that we can reach this round
            var priority = Math.max(distance - nextToMove.movementPoints, 0)
            if (priority > 0) priority += 150 //add some additional penalty for further characters so closer targets are preferred

            // adjust priority according to how likely the character is to kill the target
            val targetCharacter = targets(i)
            var avgDamage = (nextToMove.attackPower * 0.90 - targetCharacter.defensePower).toInt
            avgDamage = (avgDamage * nextToMove.characterClass.attackBonus.getOrElse(targetCharacter.characterClass.toString, 1.0)).toInt
            priority += Math.max(targetCharacter.hitpoints - avgDamage, 0)
            (priority, i)
          }})

          if (!priorities.isEmpty) {
            val targetIndex = priorities.min._2
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
  }

  override def isThinking: Boolean = calculating.get
}
