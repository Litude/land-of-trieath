package game.core

case class Coordinate(val x: Int, val y: Int) {
  // Returns non-negative neighbors, but may return neighbors outside positive map bounds!
  def neighbors: Array[Coordinate] = {
    Array(Coordinate(x - 1, y), Coordinate(x, y - 1), Coordinate(x + 1, y), Coordinate(x, y + 1))
  .filter(coordinate => coordinate.x >= 0 && coordinate.y >= 0)
  }

  // Returns the direction to the paramter coordinate acc. to which direction has the largest "distance"
  def directionTo(that: Coordinate): Direction = {
    val difference = that-this
    if (Math.abs(difference.x) > Math.abs(difference.y)) {
      if (difference.x < 0) Direction.West else Direction.East
    } else {
      if (difference.y < 0) Direction.North else Direction.South
    }
  }

  def directionToAdjacent(that: Coordinate): Direction = {
    that-this match {
      case Coordinate.North => Direction.North
      case Coordinate.South => Direction.South
      case Coordinate.West => Direction.West
      case Coordinate.East => Direction.East
      case _ => throw new IllegalArgumentException("The other coordinate must be adjacent")
    }
  }

  def geometricDistance(that: Coordinate): Double = {
    Math.sqrt(Math.pow(this.x - that.x, 2) + Math.pow(this.y - that.y, 2))
  }

  def tileDistance(that: Coordinate): Int = Math.abs(this.x - that.x) + Math.abs(this.y - that.y)

  def vectorLength: Double = Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2))

  def +(that: Coordinate): Coordinate = Coordinate(this.x + that.x, this.y + that.y)
  def +(that: Direction): Coordinate = this + Coordinate.fromDirection(that)
  def -(that: Coordinate): Coordinate = Coordinate(this.x - that.x, this.y - that.y)
  def -(that: Direction): Coordinate = this - Coordinate.fromDirection(that)
  def *(multiplier: Int): Coordinate = Coordinate(this.x * multiplier, this.y * multiplier)
}

case object Coordinate {
  val North = Coordinate(0, -1)
  val South = Coordinate(0, 1)
  val West = Coordinate(-1, 0)
  val East = Coordinate(1, 0)

  def fromDirection(direction: Direction): Coordinate = {
    direction match {
      case Direction.North => Coordinate.North
      case Direction.South => Coordinate.South
      case Direction.West => Coordinate.West
      case Direction.East => Coordinate.East
    }
  }
}
