package game.core

import scala.collection.mutable.ArrayBuffer

trait PathFinder {
  def findPath(map: Map, blockingCharacters: Seq[Character], start: Coordinate, goal: Coordinate): Option[ArrayBuffer[Coordinate]]
}
