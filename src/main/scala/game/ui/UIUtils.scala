package game.ui

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.text.Text
import scalafx.beans.property.ReadOnlyObjectProperty

import javafx.geometry.Bounds

object UIUtils {
  def getContextStringBounds(context: GraphicsContext, text: String): Bounds = {
    val metricText = new Text
    metricText.text = text
    metricText.font = context.font
    metricText.strokeWidth = context.lineWidth
    metricText.layoutBounds.get
  }
}
