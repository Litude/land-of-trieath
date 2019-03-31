package game.core

trait PlayerAI {
  //returns true if done playing turn, false otherwise
  def playTurn(game: Game): Boolean = {
    if (shouldMakeMove(game)) makeMove(game) else false
  }

  def shouldMakeMove(game: Game): Boolean = game.actionsAllowed

  def makeMove(game: Game): Boolean
}
