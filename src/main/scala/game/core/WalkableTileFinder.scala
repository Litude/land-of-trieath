package game.core

trait WalkableTileFinder {
  def findReachableTiles(map: Map, blockingCharacters: Seq[Character], start: Coordinate, distance: Int): Seq[Coordinate]
}
