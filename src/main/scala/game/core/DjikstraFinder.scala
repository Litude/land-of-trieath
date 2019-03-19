package game.core

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case object DjikstraFinder extends PathFinder with MultiPathFinder with WalkableTileFinder {

  def findPath(map: Map, blockingCharacters: Seq[Character], start: Coordinate, goal: Coordinate): Option[ArrayBuffer[Coordinate]] = {
    val nodes = performSearch(map, blockingCharacters, start, Some(Array(goal)), Int.MaxValue - 1)
    buildPathToPosition(nodes, goal)
  }

  def findPathsToPositions(map: Map, blockingCharacters: Seq[Character], start: Coordinate, goals: Seq[Coordinate]): Seq[Option[ArrayBuffer[Coordinate]]] = {
    val nodes = performSearch(map, blockingCharacters, start, Some(goals), Int.MaxValue - 1)
    goals.map(buildPathToPosition(nodes, _))
  }

  def findReachableTiles(map: Map, blockingCharacters: Seq[Character], start: Coordinate, distance: Int): Seq[Coordinate] = {
    val nodes = performSearch(map, blockingCharacters, start, None, distance)
    listReachableTiles(nodes, distance)
  }

  private def performSearch(
    map: Map, blockingCharacters: Seq[Character], start: Coordinate, goals: Option[Seq[Coordinate]], maxDistance: Int)
    : Array[Array[PathNode]] = {

    val nodes = Array.tabulate[PathNode](map.width, map.height)((x, y) => new PathNode)
    nodes(start.x)(start.y).distance = 0

    def tileIsWalkable(position: Coordinate): Boolean = {
      !map(position.x, position.y).isSolid && !blockingCharacters.exists(character => character.position == position)
    }

    //skip solid map tiles
    for {
      x <- 0 until map.width
      y <- 0 until map.height
    } {
      if (!tileIsWalkable(Coordinate(x, y))) nodes(x)(y).walkable = false
    }

    findClosestUnvisitedNode(nodes, maxDistance).foreach(traverseNode(nodes, _, goals, 0, maxDistance))
    nodes
  }

  @tailrec private def traverseNode(
    nodes: Array[Array[PathNode]], current: Coordinate, goals: Option[Seq[Coordinate]], goalsFound: Int, maxDistance: Int)
    : Unit = {

    nodes(current.x)(current.y).explored = true
    //proceed further only if we are not at a goal
    if (!goals.map(_.exists(_ == current)).getOrElse(false)) {
      updateNeighborDistances(nodes, current)
    }
    val newGoals = {
      goalsFound + (if (goals.map(_.exists(_ == current)).getOrElse(false)) 1 else 0)
    }
    if (goals.map(newGoals < _.length).getOrElse(true)) {
      findClosestUnvisitedNode(nodes, maxDistance) match {
        case Some(node) => traverseNode(nodes, node, goals, newGoals, maxDistance)
        case _ =>
      }
    }
  }

  private def updateNeighborDistances(nodes: Array[Array[PathNode]], current: Coordinate): Unit = {
    current.neighbors.filter(neighbor => neighbor.x < nodes.length && neighbor.y < nodes(0).length).foreach(neighbor => {
      if (nodes(neighbor.x)(neighbor.y).walkable) {
        val newDistance = nodes(current.x)(current.y).distance + 1
        if (newDistance < nodes(neighbor.x)(neighbor.y).distance) {
          nodes(neighbor.x)(neighbor.y).distance = newDistance
        }
      }
    })
  }

  private def findClosestUnvisitedNode(nodes: Array[Array[PathNode]], maxDistance: Int): Option[Coordinate] = {
    var minValue = Int.MaxValue
    var index: Option[Coordinate] = None

    for {
      x <- 0 until nodes.length
      y <- 0 until nodes(0).length
      if (nodes(x)(y).shouldVisit && nodes(x)(y).distance < minValue && nodes(x)(y).distance <= maxDistance)
    } {
      minValue = nodes(x)(y).distance
      index = Some(Coordinate(x, y))
    }
    index
  }

  private def buildPathToPosition(nodes: Array[Array[PathNode]], goalPosition: Coordinate): Option[ArrayBuffer[Coordinate]] = {
    if (nodes(goalPosition.x)(goalPosition.y).distance != Int.MaxValue) {
      val path = ArrayBuffer[Coordinate]()
      var current = goalPosition
      do {
        path += current
        current = current.neighbors
        .filter(neighbor => neighbor.x < nodes.length && neighbor.y < nodes(0).length)
        .minBy(neighbor => nodes(neighbor.x)(neighbor.y).distance)
      } while (nodes(current.x)(current.y).distance != 0)
      Some(path.reverse)
    } else {
      None
    }
  }

  private def listReachableTiles(nodes: Array[Array[PathNode]], maxDistance: Int): Seq[Coordinate] = {
    for {
      x <- 0 until nodes.length
      y <- 0 until nodes(0).length
      if (nodes(x)(y).explored && nodes(x)(y).distance <= maxDistance && nodes(x)(y).distance != 0)
    } yield Coordinate(x, y)
  }

  class PathNode(var distance: Int = Int.MaxValue, var explored: Boolean = false, var walkable: Boolean = true) {
    def shouldVisit: Boolean = !explored && walkable
  }
}
