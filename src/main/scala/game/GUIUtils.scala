package game

import scalafx.scene.text.Text
import scalafx.scene.canvas.GraphicsContext

object GUIUtils {
  def getContextStringBounds(context: GraphicsContext, text: String) = {
    val metricText = new Text
    metricText.text = text
    metricText.font = context.font
    metricText.strokeWidth = context.lineWidth
    metricText.layoutBounds.get
  }
}
