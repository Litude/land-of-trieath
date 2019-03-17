package game

import scala.collection.mutable.ArrayBuffer

trait WalkableTileFinder {
  def findReachableTiles(map: Map, characters: Array[Character], start: Coordinate, distance: Int): Seq[Coordinate]
}
