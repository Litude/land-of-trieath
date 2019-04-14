package game.core

abstract class PlayerAI {
  var previousMoveTime: Long = PlayerAI.UndefinedMoveTime
  //returns true if done playing turn, false otherwise
  //passes control to actual AI if the game allows actions and at least MinMovementDelay has passed since actions were reallowed
  //wait MinMovementDelay to prevent too rapid AI actions (e.g. attacking with several characters in a split second)
  final def playTurn(game: Game): Boolean = {
    if (shouldMakeMove(game)) {
      if (previousMoveTime == PlayerAI.UndefinedMoveTime) {
        previousMoveTime = System.currentTimeMillis()
        false
      } else if (System.currentTimeMillis - previousMoveTime > PlayerAI.MinMovementDelay) {
        val result = makeMove(game)
        previousMoveTime = PlayerAI.UndefinedMoveTime
        result
      } else {
        false
      }
    } else {
      false
    }
  }

  final def shouldMakeMove(game: Game): Boolean = game.actionsAllowed && !isThinking

  def makeMove(game: Game): Boolean

  def isThinking: Boolean
}

object PlayerAI {
  val UndefinedMoveTime: Long = -1
  val MinMovementDelay = 500
}
