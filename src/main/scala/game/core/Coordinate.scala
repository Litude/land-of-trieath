package game.core

case class Coordinate(val x: Int, val y: Int) {
  
  // Returns non-negative neighbors, but may return neighbors outside positive map bounds!
  def neighbors: Array[Coordinate] = {
    Array(Coordinate(x - 1, y), Coordinate(x, y - 1), Coordinate(x + 1, y), Coordinate(x, y + 1))
  .filter(coordinate => coordinate.x >= 0 && coordinate.y >= 0)
  }
  
  def directionTo(that: Coordinate): Direction.Value = {
    this-that match {
      case Coordinate(x, y) => {
        if (x == 1) Direction.West
        else if (x == -1) Direction.East
        else if (y == 1) Direction.North
        else Direction.South
      }
    }
  }
  
  def geometricDistance(that: Coordinate): Double = {
    Math.sqrt(Math.pow(this.x - that.x, 2) + Math.pow(this.y - that.y, 2))
  }
  
  def tileDistance(that: Coordinate): Int = Math.abs(this.x - that.x) + Math.abs(this.y - that.y)
  
  def +(that: Coordinate) = Coordinate(this.x + that.x, this.y + that.y)
  def -(that: Coordinate) = Coordinate(this.x - that.x, this.y - that.y)
  def *(multiplier: Int) = Coordinate(this.x * multiplier, this.y * multiplier)
}

case object Coordinate {
  
  def fromDirection(direction: Direction.Value): Coordinate = {
    direction match {
      case Direction.West => Coordinate(-1, 0)
      case Direction.East => Coordinate(1, 0)
      case Direction.North => Coordinate(0, -1)
      case Direction.South => Coordinate(0, 1)
    }
  }
}
