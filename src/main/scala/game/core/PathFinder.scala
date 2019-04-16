package game.core

import scala.collection.mutable.ArrayBuffer

trait PathFinder {
  //used when walking to a destination tile
  def findPath(map: Map, blockingCharacters: Seq[Character], start: Coordinate, goal: Coordinate): Option[ArrayBuffer[Coordinate]]

  //used when attacking an enemy, finds the closest tile which is attackerRange tiles away from the actual goal
  def findPathToTarget(
    map: Map, blockingCharacters: Seq[Character], start: Coordinate, goal: Coordinate, attackerRange: Int)
    : Option[ArrayBuffer[Coordinate]]

  def findDistancesToPositions(map: Map, blockingCharacters: Seq[Character], start: Coordinate, goals: Seq[Coordinate]): Seq[Int]
}
