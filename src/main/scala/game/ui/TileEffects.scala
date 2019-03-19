package game.ui

import scala.collection.mutable.ArrayBuffer
import scalafx.scene.canvas._
import scalafx.scene.paint.Color._

import game.core.Coordinate
import game.core.Tile

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

trait TileEffect {
  var ttl: Int
  val singleInstanceOnly = false //allows only the most recent effect to appear
  def position: Coordinate
  def update(passedTime: Int): Unit = {
    ttl -= passedTime
  }
  def isAlive: Boolean = ttl > 0
  def draw(canvas: Canvas): Unit
}

class HighlightEffect(val position: Coordinate) extends TileEffect {
  val BlinkDelay = 300
  val OriginalTTL = 900

  var ttl = OriginalTTL
  var blinkRemaining = BlinkDelay
  var visible = true
  override val singleInstanceOnly = true

  override def update(passedTime: Int): Unit = {
    super.update(passedTime)
    blinkRemaining -= passedTime
    if (blinkRemaining <= 0) {
      blinkRemaining = BlinkDelay
      visible = !visible
    }
  }

  override def draw(canvas: Canvas): Unit = {
    if (visible) {
      val context = canvas.graphicsContext2D
      context.save()
      context.stroke = GreenYellow
      context.globalAlpha = 0.8
      context.lineWidth = 1
      context.strokeRect(position.x * Tile.Size, position.y * Tile.Size, Tile.Size, Tile.Size)
      context.restore()
    }
  }
}

class DamageCounter(val damage: Int, val position: Coordinate) extends TileEffect {
  val OriginalTTL = 1000

  var offset = 0.0
  var ttl = OriginalTTL

  override def update(passedTime: Int): Unit = {
    super.update(passedTime)
    offset += passedTime / 20.0
  }

  override def draw(canvas: Canvas): Unit = {
    val context = canvas.graphicsContext2D
    context.save()
    context.fill = Red
    context.stroke = Black
    context.lineWidth = 2

    val string = damage.toString
    val bounds = Utils.getContextStringBounds(context, string)
    val textWidth = bounds.getWidth
    val textHeight = bounds.getHeight

    context.strokeText(string,
        position.x * Tile.Size + (Tile.Size - textWidth) / 2, position.y * Tile.Size + (Tile.Size + textHeight) / 2 - offset, Tile.Size
        )
    context.fillText(string,
        position.x * Tile.Size + (Tile.Size - textWidth) / 2, position.y * Tile.Size + (Tile.Size + textHeight) / 2 - offset, Tile.Size
        )
    context.restore()
  }
}

