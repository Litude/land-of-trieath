package game.core

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object MapGenerator {
  val MapObjects = MapObject.readFromFile("definitions/map_objects.json").filter(!_.randomDisallowed)
  val ObjectPlacementRatio = 4
  val SpawnWidth = 12
  val SpawnHeight = 6

  def generateMap(width: Int, height: Int, numPlayers: Int): Map = {
    val map = new Map(width, height)
    val playerPositions = generatePlayerPositions(numPlayers)
    map.spawns = generateCharacterSpawns(playerPositions, width, height)
    val corridors = generateCorridors(playerPositions, width, height)
    map.corridors = corridors
    placeMapObjects(map, corridors)
    map
  }

  private def placeMapObjects(map: Map, corridors: Seq[Coordinate]): Unit = {
    for {
      y <- 0 until map.height
      x <- 0 until map.width
      if (Random.nextInt(ObjectPlacementRatio) == 0 && !map(x, y).isSolid && !isNextToSpawn(map, x, y))
    } {
      val mapObject = MapObjects(Random.nextInt(MapObjects.length))
      if (isAreaFreeForObjectPlacement(map, x, y, mapObject, corridors)) {
        map.placeObject(mapObject, x, y)
      }
    }
  }

  private def isNextToSpawn(map: Map, x: Int, y: Int): Boolean = {
    map.spawns.flatten.exists(_.position.tileDistance(Coordinate(x, y)) <= 3)
  }

  private def isAreaFreeForObjectPlacement(map: Map, x: Int, y: Int, mapObject: MapObject, corridors: Seq[Coordinate]): Boolean = {
    def squareIsCorridor(column: Int, row: Int): Boolean = corridors.exists(_ == Coordinate(x + column, y + row))
    def atWidthBounds(column: Int, row: Int): Boolean = column == mapObject.width || (x + column == map.width)
    def atHeightBounds(column: Int, row: Int): Boolean = row == mapObject.height || (y + row == map.height)
    def squareShouldBeFree(column: Int, row: Int): Boolean = {
      map(x + column, y + row).isSolid || isNextToSpawn(map, x + column, y + row) || squareIsCorridor(column, row)
    }

    @tailrec def traverseArea(column: Int, row: Int): Boolean = {
      (column, row) match {
        case (column, row) if atWidthBounds(column, row) => traverseArea(0, row + 1)
        case (column, row) if atHeightBounds(column, row) => true
        case (column, row) if (mapObject.tiles(column)(row).isDefined && squareShouldBeFree(column, row)) => false
        case (column, row) => traverseArea(column + 1, row)
      }
    }
    traverseArea(0, 0)
  }

  private def generatePlayerPositions(numPlayers: Int): Seq[Direction] = {
    if (numPlayers != 2) {
      Random.shuffle(Direction.values).take(numPlayers)
    } else {
      val direction = Direction.values(Random.nextInt(Direction.NumDirections))
      Seq(direction, direction.opposite)
    }
  }

  private def generateCharacterSpawns(playerPositions: Seq[Direction], width: Int, height: Int): Seq[Seq[CharacterSpawn]] = {
    def alternatingOffset(i: Int, extent: Int): Int = {
      val multiplier = if (i % 2 == 1) 1 else -1
      val offset = i / 2
      (extent / 2) + 2 * multiplier * offset
    }

    playerPositions.map(direction => {
      for {
        j <- 0 until 2
        i <- 1 to 6
      } yield (direction match  {
        case Direction.North => CharacterSpawn(Coordinate(alternatingOffset(i, width), 3 - (j * 2)), Direction.South)
        case Direction.West => CharacterSpawn(Coordinate(3 - (j * 2), alternatingOffset(i, height)), Direction.East)
        case Direction.South => CharacterSpawn(Coordinate(alternatingOffset(i, width), height - 4 + (j * 2)), Direction.North)
        case Direction.East => CharacterSpawn(Coordinate(width - 4 + (j * 2), alternatingOffset(i, height)), Direction.West)
      })
    })
  }

  private def generateCorridors(playerPositions: Seq[Direction], width: Int, height: Int): Seq[Coordinate] = {
    (for {
      i <- 0 until playerPositions.length - 1
      j <- 1 + i until playerPositions.length
    } yield (playerPositions(i).orientationTo(playerPositions(j)) match {
      case Orientation.Opposite => corridorAcross(playerPositions(i), width, height)
      case _ => Seq()
    })).flatten
  }

  private def corridorCarverStartPosition(direction: Direction, width: Int, height: Int): Coordinate = {
    direction match {
      case Direction.North => Coordinate(width / 2 - SpawnWidth / 2 + Random.nextInt(SpawnWidth), SpawnHeight - 1)
      case Direction.South => Coordinate(width / 2 - SpawnWidth / 2 + Random.nextInt(SpawnWidth), height - SpawnHeight)
      case Direction.East => Coordinate(width - SpawnHeight, height / 2 - SpawnWidth / 2 + Random.nextInt(SpawnWidth))
      case Direction.West => Coordinate(SpawnHeight - 1, height / 2 - SpawnWidth / 2 + Random.nextInt(SpawnWidth))
    }
  }

  private def atCorridorCarverEndPosition(direction: Direction, position: Coordinate, width: Int, height: Int): Boolean = {
    direction match {
      case Direction.North => position.y == height - SpawnHeight
      case Direction.South => position.y == SpawnHeight - 1
      case Direction.East => position.x == SpawnHeight - 1
      case Direction.West => position.x == width - SpawnHeight
    }
  }

  private def corridorForwardProbability(direction: Direction, width: Int, height: Int, next: Coordinate, middle: Coordinate): Double = {
    if (direction.isVertical) {
      val difference = Math.abs((next.x - middle.x) / (width / 2.0))
        if (direction == Direction.South && next.y > middle.y || direction == Direction.North && next.y <= middle.y) {
          Utils.clamp(difference, 0.0, 1.0)
        } else {
          1.0 - Utils.clamp(difference, 0.0, 1.0)
        }
      }
    else {
      val difference = Math.abs((next.y - middle.y) / (height / 2.0))
        if (direction == Direction.East && next.x > middle.x || direction == Direction.West && next.x <= middle.x) {
          Utils.clamp(difference, 0.0, 1.0)
        } else {
          1.0 - Utils.clamp(difference, 0.0, 1.0)
        }
    }
  }

  private def corridorRotationDirection(initialDirection: Direction, currentDirection: Direction, next: Coordinate, middle: Coordinate): Direction = {
    if (initialDirection.isVertical) {
      if (next.y > middle.y) {
        if (next.x > middle.x) currentDirection.clockwise else currentDirection.counterclockwise
      } else {
        if (next.x > middle.x) currentDirection.counterclockwise else currentDirection.clockwise
      }
    } else {
      if (next.x > middle.x) {
        if (next.y > middle.y) currentDirection.counterclockwise else currentDirection.clockwise
      } else {
        if (next.y > middle.y) currentDirection.clockwise else currentDirection.counterclockwise
      }
    }
  }

  private def corridorAcross(direction: Direction, width: Int, height: Int): Seq[Coordinate] = {
    val start = corridorCarverStartPosition(direction, width, height)
    val coordinates = ArrayBuffer[Coordinate](start)
    val middle = Coordinate(width / 2, height / 2)
    val initialDirection: Direction = direction.opposite

    var currentDirection: Direction = initialDirection
    var next = start + currentDirection
    do {
      coordinates += next
      val probability = corridorForwardProbability(direction, width, height, next, middle)
      if (currentDirection != initialDirection) {
        if (Random.nextDouble < probability) {
          currentDirection = initialDirection
        }
      } else if (Random.nextDouble < 1.0 - probability) {
        currentDirection = corridorRotationDirection(direction, currentDirection, next, middle)
      }

      next += currentDirection
    } while (!atCorridorCarverEndPosition(direction, next, width, height))
    coordinates += next
    coordinates
  }
}
