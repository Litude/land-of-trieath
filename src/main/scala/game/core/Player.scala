package game.core

import scala.collection.mutable.ArrayBuffer

sealed trait PlayerType

object PlayerType {
  case object Human extends PlayerType
  case object Computer extends PlayerType
}

abstract class Player(val playerType: PlayerType, val characters: ArrayBuffer[Character]) {

  def this(playerType: PlayerType) {
    this(playerType, ArrayBuffer())
  }

  def isAlive: Boolean = !characters.isEmpty

  def copy: Player

}

class HumanPlayer(characters: ArrayBuffer[Character]) extends Player(PlayerType.Human, characters) {
  def this() {
    this(ArrayBuffer())
  }

  def copy: Player = new HumanPlayer(characters.map(_.copy))
}

class AIPlayer(characters: ArrayBuffer[Character], val ai: PlayerAI) extends Player(PlayerType.Computer, characters) {

  def this(ai: PlayerAI) {
    this(ArrayBuffer(), ai)
  }

  def this(characters: ArrayBuffer[Character]) {
    this(characters, new BasicAI)
  }

  def copy: Player = new AIPlayer(characters.map(_.copy))
}

