package game.core

class Map(val width: Int, val height: Int) {
  val tiles: Array[Array[Tile]] = Array.tabulate[Tile](width, height)((x, y) => new Tile(x % 4, None))
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
}

object Map {
  val TestMapSize = 40
}

case class CharacterSpawn(position: Coordinate, direction: Direction)
