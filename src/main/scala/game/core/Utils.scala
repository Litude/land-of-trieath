package game.core

import scala.annotation.tailrec

case object Utils {
  def find2DArrayEntry[T](array: Array[Array[T]], predicate: T => Boolean): Option[T] = {
    find2DArrayIndex(array, predicate) match {
      case (x, y) if (x == -1 && y == -1) => None
      case (x, y) => Some(array(x)(y))
    }
  }

  def find2DArrayIndex[T](array: Array[Array[T]], predicate: T => Boolean): (Int, Int) = {
    @tailrec def findRecurser(x: Int, y: Int): (Int, Int) = {
      (x, y) match {
        case (x, y) if (x == array.length) => (-1, -1)
        case (x, y) if (y == array(x).length) => findRecurser(x + 1, 0)
        case (x, y) if (predicate(array(x)(y))) => (x, y)
        case _ => findRecurser(x, y + 1)
      }
    }
    findRecurser(0, 0)
  }

  def clamp(value: Double, min: Double, max: Double): Double = Math.max(Math.min(value, max), min)
}
