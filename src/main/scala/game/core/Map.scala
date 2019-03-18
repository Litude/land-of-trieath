package game.core

class Map(val width: Int, val height: Int) {
  val tiles: Array[Array[Tile]] = Array.tabulate[Tile](width, height)((x, y) => new Tile(x % 4, if (x == y && x > 0) Some(0) else None))
  
  def apply(x: Int, y: Int) = tiles(x)(y)
  
}
