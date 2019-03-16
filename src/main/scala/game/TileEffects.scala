package game

import scala.collection.mutable.ArrayBuffer
import scalafx.scene.canvas._
import scalafx.scene.paint.Color._

class TileEffects {
  private var effects = ArrayBuffer[TileEffect]()
  
  def +=(newEffect: TileEffect) = {
    if (newEffect.singleInstanceOnly) {
      effects = effects.filter(_.getClass != newEffect.getClass) :+ newEffect
    } else {
      effects += newEffect
    }
  }
  
  def update(passedTime: Int): Unit = {
    effects.foreach(_.update(passedTime))
    effects = effects.filter(_.ttl > 0)
  }
  
  def draw(canvas: Canvas): Unit = {
    effects.foreach(_.draw(canvas))
  }
}

trait TileEffect {
  var ttl: Int
  val singleInstanceOnly = false //allows only the most recent effect to appear
  def position: Coordinate
  def update(passedTime: Int) = {
    ttl -= passedTime
  }
  def isAlive: Boolean = ttl > 0
  def draw(canvas: Canvas): Unit
}

class HighlightEffect(val position: Coordinate) extends TileEffect {
  val BlinkDelay = 200
  var ttl = 900
  var blinkRemaining = 300
  var visible = true
  override val singleInstanceOnly = true

  override def update(passedTime: Int) = {
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
  var offset = 0.0
  var ttl = 1000
  
  override def update(passedTime: Int) = {
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
    val bounds = GUIUtils.getContextStringBounds(context, string)
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

