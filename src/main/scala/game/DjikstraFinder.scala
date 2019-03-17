package game

import scala.collection.mutable.ArrayBuffer

class DjikstraFinder extends PathFinder with MultiPathFinder with WalkableTileFinder {
  
  def findPath(map: Map, characters: Array[Character], start: Coordinate, goal: Coordinate): Option[ArrayBuffer[Coordinate]] = {
    val nodes = performSearch(map, characters, start, Some(Array(goal)), Int.MaxValue - 1)
    buildPathToPosition(nodes, goal)
  }
  
  def findReachableTiles(map: Map, characters: Array[Character], start: Coordinate, distance: Int): Seq[Coordinate] = {
    val nodes = performSearch(map, characters, start, None, distance)
    listReachableTiles(nodes, distance)
  }
  
  def findPathsToPositions(map: Map, characters: Array[Character], start: Coordinate, goals: Array[Coordinate]): Array[Option[ArrayBuffer[Coordinate]]] = {
    val nodes = performSearch(map, characters, start, Some(goals), Int.MaxValue - 1)
    goals.map(buildPathToPosition(nodes, _))
  }
  
  def performSearch(map: Map, characters: Array[Character], start: Coordinate, goals: Option[Array[Coordinate]], maxDistance: Int): Array[Array[PathNode]] = {
    
    val nodes = Array.tabulate[PathNode](map.width, map.height)((x, y) => new PathNode)
    nodes(start.x)(start.y).distance = 0
    
    var goalsFound = 0
    var searchFailed = false
    
    def tileIsWalkable(position: Coordinate): Boolean = {
      !map(position.x, position.y).isSolid && !characters.exists(character => character.position == position)
    }
    
    def findClosestUnvisitedNode: Option[Coordinate] = {
      val minValue = nodes.flatten.filter(!_.explored).minBy(_.distance).distance
      if (minValue > maxDistance) {
        None
      } else {
        for {
          x <- 0 until map.width
          y <- 0 until map.height
          if (!nodes(x)(y).explored && nodes(x)(y).distance == minValue)
        } return Some(Coordinate(x, y))
      }
      None
    }
    
    //skip solid map tiles
    for {
      x <- 0 until map.width
      y <- 0 until map.height
    } {
      if (!tileIsWalkable(Coordinate(x, y))) nodes(x)(y).explored = true
    }
    
    do {
      findClosestUnvisitedNode match {
        case Some(current) => {
          nodes(current.x)(current.y).explored = true
          goals match {
            case Some(goalPositions) if (goalPositions.exists(_ == current)) => goalsFound += 1
            case _ => {
              current.neighbors.filter(neighbor => neighbor.x < map.width && neighbor.y < map.height).foreach(neighbor => {
                if (tileIsWalkable(neighbor)) {
                  val newDistance = nodes(current.x)(current.y).distance + 1
                  if (newDistance < nodes(neighbor.x)(neighbor.y).distance) nodes(neighbor.x)(neighbor.y).distance = newDistance
                }
              })
            }
          }
        }
        case None => searchFailed = true
      }
    } while (!searchFailed && goals.map(goalsFound < _.length).getOrElse(true) && nodes.flatten.exists(!_.explored))
    nodes
  }
  
  def buildPathToPosition(nodes: Array[Array[PathNode]], goalPosition: Coordinate): Option[ArrayBuffer[Coordinate]] = {
    if (nodes(goalPosition.x)(goalPosition.y).distance != Int.MaxValue) {
      val path = ArrayBuffer[Coordinate]()
      var current = goalPosition
      do {
        path += current
        current = current.neighbors.filter(neighbor => neighbor.x < nodes.length && neighbor.y < nodes(0).length).minBy(neighbor => nodes(neighbor.x)(neighbor.y).distance)
      } while (nodes(current.x)(current.y).distance != 0)
      Some(path.reverse)
    } else {
      None
    }
  }
  
  def listReachableTiles(nodes: Array[Array[PathNode]], maxDistance: Int): Seq[Coordinate] = {
    for {
      x <- 0 until nodes.length
      y <- 0 until nodes(0).length
      if (nodes(x)(y).explored && nodes(x)(y).distance <= maxDistance && nodes(x)(y).distance != 0)
    } yield Coordinate(x, y)
  }
}

class PathNode(var distance: Int = Int.MaxValue, var explored: Boolean = false)

