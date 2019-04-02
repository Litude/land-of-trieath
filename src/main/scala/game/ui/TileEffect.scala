package game.ui

import scalafx.scene.canvas._
import scalafx.scene.paint.Color._

import game.core.Coordinate
import game.core.Tile

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
    val bounds = UIUtils.getContextStringBounds(context, string)
    val textWidth = bounds.getWidth
    val textHeight = bounds.getHeight

    val xPos = position.x * Tile.Size + (Tile.Size - textWidth) / 2
    val yPos = position.y * Tile.Size + (Tile.Size + textHeight) / 2 - offset

    context.strokeText(string, xPos, yPos, Tile.Size)
    context.fillText(string, xPos, yPos, Tile.Size)
    context.restore()
  }
}
