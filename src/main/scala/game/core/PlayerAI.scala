package game.core

trait PlayerAI {

  var previousMoveTime: Long
  //returns true if done playing turn, false otherwise
  def playTurn(game: Game): Boolean = {
    if (shouldMakeMove(game)) makeMove(game) else false
  }

  def shouldMakeMove(game: Game): Boolean = (System.currentTimeMillis - previousMoveTime > PlayerAI.MinMovementDelay) && game.actionsAllowed

  def makeMove(game: Game): Boolean
}

object PlayerAI {
  val MinMovementDelay = 500
}
