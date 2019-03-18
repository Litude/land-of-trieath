package game.ui

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.text.Text
import scalafx.beans.property.ReadOnlyObjectProperty

object Utils {
  def getContextStringBounds(context: GraphicsContext, text: String) = {
    val metricText = new Text
    metricText.text = text
    metricText.font = context.font
    metricText.strokeWidth = context.lineWidth
    metricText.layoutBounds.get
  }
}
