package game.core

import scala.collection.mutable.ArrayBuffer

trait PathFinder {
  def findPath(map: Map, blockingCharacters: Seq[Character], start: Coordinate, goal: Coordinate): Option[ArrayBuffer[Coordinate]]

  def findPathToTarget(
    map: Map, blockingCharacters: Seq[Character], start: Coordinate, goal: Coordinate, attackerRange: Int)
    : Option[ArrayBuffer[Coordinate]]

  def findDistancesToPositions(map: Map, blockingCharacters: Seq[Character], start: Coordinate, goals: Seq[Coordinate]): Seq[Int]
}
