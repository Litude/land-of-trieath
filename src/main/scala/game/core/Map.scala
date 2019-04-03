package game.core

import java.io._

import scala.util.{Failure, Success, Try}

class Map(val width: Int, val height: Int, val tiles: Array[Array[Tile]]) {

  def this(mapWidth: Int, mapHeight: Int) {
    this(mapWidth, mapHeight, Array.tabulate[Tile](mapWidth, mapHeight)((x, y) => new Tile(x % 4, None)))
  }

  var spawns: Seq[Seq[CharacterSpawn]] = Seq(Seq())
  var corridors: Seq[Coordinate] = Seq() //stored in map for debug drawing purposes only

  def placeObject(mapObject: MapObject, x: Int, y: Int) {
    for {
      i <- 0 until mapObject.width
      j <- 0 until mapObject.height
      if (x + i < width && y + j < height)
    } {
      this(x + i, y + j).obstacleType = mapObject.tiles(i)(j)
    }
  }

  def apply(x: Int, y: Int): Tile = tiles(x)(y)
  def update(x: Int, y: Int, tile: Tile): Unit = tiles(x)(y) = tile

  def writeToFile(filename: String): Boolean = {
    Try(new FileOutputStream(filename)).map(new BufferedOutputStream(_)) match {
      case Success(stream) => {
        try {
          stream.write(Map.HeaderMagic.getBytes)
          stream.write(Array[Byte](this.width.toByte, this.height.toByte, this.spawns.length.toByte))
          for {
            y <- 0 until this.height
            x <- 0 until this.width
          } {
            val tile = this(x, y)
            stream.write(tile.groundType.toByte)
            stream.write(tile.obstacleType.getOrElse(Tile.UndefinedObjectValue))
          }
          this.spawns.foreach(playerSpawns => {
            stream.write(playerSpawns.length.toByte)
            stream.write(playerSpawns.map(_.getBytes).flatten.toArray)
          })
          true
        } catch {
          case e: IOException => false
        } finally {
          stream.close()
        }
      }
      case Failure(_) => false
    }
  }
}

object Map {
  val Directory = "map"
  val HeaderMagic = "LTRIEATH10"

  def readFromFile(filename: String): Option[Map] = {
    Try(new FileInputStream(filename)).map(new BufferedInputStream(_)) match {
      case Success(stream) => {
        try {
          val headerBuffer = Array.ofDim[Byte](HeaderMagic.length)
          stream.read(headerBuffer)
          if (HeaderMagic == new String(headerBuffer)) {
            val width = stream.read()
            val height = stream.read()
            val numPlayers = stream.read()
            val tiles = Array.fill[Tile](height, width)(
              new Tile(stream.read(), {
                val value = stream.read()
                if (value != Tile.UndefinedObjectValue) Some(value) else None
              })
            ).transpose
            val spawns = (0 until numPlayers).map(_ => {
              val numSpawns = stream.read()
              val buffer = Array.ofDim[Byte](CharacterSpawn.SerializedSize)
              (0 until numSpawns).map(_ => {
                stream.read(buffer)
                CharacterSpawn.fromBytes(buffer)
              })
            })
            val map = new Map(width, height, tiles)
            map.spawns = spawns
            Some(map)
          } else {
            None
          }
        } catch {
          case e: IOException => None
        } finally {
          stream.close()
        }
      }
      case Failure(_) => None
    }
  }
}

case class CharacterSpawn(position: Coordinate, direction: Direction) {
  def getBytes: Array[Byte] = Array(position.x.toByte, position.y.toByte, direction.toByte)
}

object CharacterSpawn {
  val SerializedSize = 3
  def fromBytes(bytes: Array[Byte]): CharacterSpawn = CharacterSpawn(Coordinate(bytes(0), bytes(1)), Direction.fromByte(bytes(2)))
}
