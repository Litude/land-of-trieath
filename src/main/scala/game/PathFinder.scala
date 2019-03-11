package game

import scala.collection.mutable.ArrayBuffer

trait PathFinder {
  def findPath(map: Map, characters: Array[Character], start: Coordinate, goal: Coordinate): Option[ArrayBuffer[Coordinate]]
}
