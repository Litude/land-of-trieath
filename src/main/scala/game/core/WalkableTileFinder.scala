package game.core

trait WalkableTileFinder {
  def findReachableTiles(
    map: Map, friendlyCharacters: Seq[Character], blockingCharacters: Seq[Character], start: Coordinate, distance: Int)
    : Seq[Coordinate]
}
