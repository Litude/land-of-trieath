package game.core

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case object DjikstraFinder extends PathFinder with WalkableTileFinder {
  val TraverseAllTiles = -1

  def findPath(map: Map, blockingCharacters: Seq[Character], start: Coordinate, goal: Coordinate): Option[ArrayBuffer[Coordinate]] = {
    val nodes = buildNodeMap(map, blockingCharacters, Seq(goal))
    performSearch(nodes, start)
    buildPathToPosition(nodes, goal)
  }

  def findPathToTarget(
    map: Map, blockingCharacters: Seq[Character], start: Coordinate, goal: Coordinate, attackerRange: Int)
    : Option[ArrayBuffer[Coordinate]] = {
    val nodes = buildNodeMapForRange(map, blockingCharacters, goal, attackerRange)
    //if we stop searching when we hit the first goal, it is guaranteed to have the shortest distance
    performSearch(nodes, start)
    findMatchingNode(nodes, (node) => node.visited && node.goal) match {
      case Some(position) => buildPathToPosition(nodes, position)
      case _ => None
    }
  }

  def findDistancesToPositions(map: Map, blockingCharacters: Seq[Character], start: Coordinate, goals: Seq[Coordinate]): Seq[Int] = {
    val nodes = buildNodeMap(map, blockingCharacters, goals)
    performSearch(nodes, start, goals.length)
    goals.map(coord => nodes(coord.x)(coord.y).distance)
  }

  def findReachableTiles(
    map: Map, friendlyCharacters: Seq[Character], blockingCharacters: Seq[Character], start: Coordinate, distance: Int)
    : Seq[Coordinate] = {
    //consider friendly characters blocking, enemy characters as goals so they will be highlighted but the search won't traverse futher
    val nodes = buildNodeMap(map, friendlyCharacters, blockingCharacters.map(_.position))
    performSearch(nodes, start, TraverseAllTiles, distance)
    listReachableTiles(nodes, distance)
  }

  private def buildNodeMapForRange(map: Map, blockingCharacters: Seq[Character], target: Coordinate, attackerRange: Int) = {
    val nodes = Array.tabulate[PathNode](map.width, map.height)((x, y) => {
      if (Coordinate(x, y).tileDistance(target) <= attackerRange) new PathNode(true) else new PathNode
    })
    markUnwalkableTiles(nodes, map, blockingCharacters)
    nodes
  }

  private def buildNodeMap(map: Map, blockingCharacters: Seq[Character], goals: Seq[Coordinate]) = {
    val nodes = Array.fill[PathNode](map.width, map.height)(new PathNode)
    goals.foreach(goal => nodes(goal.x)(goal.y).goal = true)
    markUnwalkableTiles(nodes, map, blockingCharacters)
    nodes
  }

  private def markUnwalkableTiles(nodes: Array[Array[PathNode]], map: Map, blockingCharacters: Seq[Character]): Unit = {
    def tileIsWalkable(position: Coordinate): Boolean = {
      !map(position.x, position.y).isSolid && !blockingCharacters.exists(character => character.position == position)
    }
    for {
      x <- 0 until map.width
      y <- 0 until map.height
    } {
    if (!tileIsWalkable(Coordinate(x, y))) nodes(x)(y).walkable = false
    }
  }

  private def performSearch(
    nodes: Array[Array[PathNode]], start: Coordinate, goalsToFind: Int = 1, maxDistance: Int = Int.MaxValue - 1)
    : Unit = {
    nodes(start.x)(start.y).distance = 0
    findClosestUnvisitedNode(nodes, maxDistance).foreach(traverseNode(nodes, _, goalsToFind, 0, maxDistance))
  }

  @tailrec private def traverseNode(
    nodes: Array[Array[PathNode]], current: Coordinate, goalsToFind: Int, goalsFound: Int, maxDistance: Int)
    : Unit = {

    nodes(current.x)(current.y).visited = true

    var currentlyFound = goalsFound
    //proceed further only if we are not at a goal
    if (!nodes(current.x)(current.y).goal) {
      updateNeighborDistances(nodes, current)
    } else {
      currentlyFound += 1
    }
    if (goalsToFind == TraverseAllTiles || currentlyFound < goalsToFind) {
      findClosestUnvisitedNode(nodes, maxDistance) match {
        case Some(node) => traverseNode(nodes, node, goalsToFind, currentlyFound, maxDistance)
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

  private def findMatchingNode(nodes: Array[Array[PathNode]], predicate: PathNode => Boolean): Option[Coordinate] = {
    val index = Utils.find2DArrayIndex(nodes, predicate)
    if (index == (-1, -1)) None else Some(Coordinate(index._1, index._2))
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
      if (nodes(x)(y).visited && nodes(x)(y).distance <= maxDistance && nodes(x)(y).distance != 0)
    } yield Coordinate(x, y)
  }

  class PathNode(var distance: Int = Int.MaxValue, var visited: Boolean = false, var walkable: Boolean = true, var goal: Boolean = false) {
    def this(goal: Boolean) = {
      this(Int.MaxValue, false, true, goal)
    }
    def shouldVisit: Boolean = !visited && walkable
  }
}
