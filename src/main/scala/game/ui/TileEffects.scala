package game.ui

import scala.collection.mutable.ArrayBuffer
import scalafx.scene.canvas._

class TileEffects {
  private var _effects = ArrayBuffer[TileEffect]()

  def +=(newEffect: TileEffect): Unit = {
    if (newEffect.singleInstanceOnly) {
      _effects = _effects.filter(_.getClass != newEffect.getClass) :+ newEffect
    } else {
      _effects += newEffect
    }
  }

  def update(passedTime: Int): Unit = {
    _effects.foreach(_.update(passedTime))
    _effects = _effects.filter(_.ttl > 0)
  }

  def draw(canvas: Canvas): Unit = {
    _effects.foreach(_.draw(canvas))
  }
}
