package game

import scala.collection.mutable.ArrayBuffer

trait MultiPathFinder {
  def findPathsToPositions(map: Map, characters: Array[Character], start: Coordinate, goals: Array[Coordinate]): Array[Option[ArrayBuffer[Coordinate]]]
}
