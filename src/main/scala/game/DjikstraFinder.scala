package game

import scala.collection.mutable.ArrayBuffer

class DjikstraFinder extends PathFinder with WalkableTileFinder {
  
  def findPath(map: Map, characters: Array[Character], start: Coordinate, goal: Coordinate): Option[ArrayBuffer[Coordinate]] = {
    performSearch(map, characters, start, Some(goal), Int.MaxValue - 1)
  }
  
  def findReachableTiles(map: Map, characters: Array[Character], start: Coordinate, distance: Int): ArrayBuffer[Coordinate] = {
    performSearch(map, characters, start, None, distance) match {
      case Some(result) => result
      case None => ArrayBuffer[Coordinate]()
    }
  }
  
  def performSearch(map: Map, characters: Array[Character], start: Coordinate, goal: Option[Coordinate], maxDistance: Int): Option[ArrayBuffer[Coordinate]] = {
    
    val nodes = Array.tabulate[PathNode](map.width, map.height)((x, y) => new PathNode)
    nodes(start.x)(start.y).distance = 0
    
    var goalFound = false
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
          goal match {
            case Some(goalPosition) if (goalPosition == current) => goalFound = true
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
    } while (!searchFailed && !goalFound && nodes.flatten.exists(!_.explored))
      
    goal match {
      case Some(goalPosition) => {
        if (goalFound) {
          val path = ArrayBuffer[Coordinate]()
          var current = goalPosition
          do {
            path += current
            current = current.neighbors.filter(neighbor => neighbor.x < map.width && neighbor.y < map.height).minBy(neighbor => nodes(neighbor.x)(neighbor.y).distance)
          } while (current != start)
          Some(path.reverse)
        } else {
          None
        }
      }
      case None => {
        val reachableTiles = (for {
          x <- 0 until map.width
          y <- 0 until map.height
          if (nodes(x)(y).explored && nodes(x)(y).distance <= maxDistance)
        } yield Coordinate(x, y)).filter(_ != start).to[ArrayBuffer]
        
        if (reachableTiles.isEmpty) None else Some(reachableTiles)
      }
    }
    
  }
}

class PathNode(var distance: Int = Int.MaxValue, var explored: Boolean = false)

