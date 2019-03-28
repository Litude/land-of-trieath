package game.core

sealed trait Orientation
object Orientation {
  case object Equal extends Orientation
  case object Clockwise extends Orientation
  case object Opposite extends Orientation
  case object Counterclockwise extends Orientation
}

sealed trait Direction {
  val id: Int
  val orientationMap: scala.collection.Map[Orientation, Direction]
  final def opposite: Direction = orientationMap(Orientation.Opposite)
  final def clockwise: Direction = orientationMap(Orientation.Clockwise)
  final def counterclockwise: Direction = orientationMap(Orientation.Counterclockwise)
  final def orientationTo(that: Direction): Orientation = orientationMap.map(_.swap).getOrElse(that, Orientation.Equal)
  val isVertical: Boolean
  final def isHorizontal: Boolean = !isVertical
}
object Direction {
  val NumDirections = 4
  val Values: Seq[Direction] = Seq(North, East, South, West)

  case object North extends Direction {
    val id = 0
    val orientationMap = scala.collection.Map[Orientation, Direction](
      Orientation.Clockwise -> East,
      Orientation.Opposite -> South,
      Orientation.Counterclockwise -> West
    )
    val isVertical = true
  }
  case object East extends Direction {
    val id = 1
    val orientationMap = scala.collection.Map[Orientation, Direction](
      Orientation.Clockwise -> South,
      Orientation.Opposite -> West,
      Orientation.Counterclockwise -> North
    )
    val isVertical = false
  }
  case object South extends Direction {
    val id = 2
    val orientationMap = scala.collection.Map[Orientation, Direction](
      Orientation.Clockwise -> West,
      Orientation.Opposite -> North,
      Orientation.Counterclockwise -> East
    )
    val isVertical = true
  }
  case object West extends Direction {
    val id = 3
    val orientationMap = scala.collection.Map[Orientation, Direction](
      Orientation.Clockwise -> North,
      Orientation.Opposite -> East,
      Orientation.Counterclockwise -> South
    )
    val isVertical = false
  }

}
