package game.core

import scala.collection.mutable.ArrayBuffer

trait MultiPathFinder {
  def findPathsToPositions(map: Map, blockingCharacters: Seq[Character], start: Coordinate, goals: Seq[Coordinate]): Seq[Option[ArrayBuffer[Coordinate]]]
}
