package game.ui

import javafx.geometry.Bounds

import scalafx.beans.property.ReadOnlyObjectProperty
import scalafx.scene._
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.image._
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.text.Text

object UIUtils {
  def getContextStringBounds(context: GraphicsContext, text: String): Bounds = {
    val metricText = new Text
    metricText.text = text
    metricText.font = context.font
    metricText.strokeWidth = context.lineWidth
    metricText.layoutBounds.get
  }

  def rotateImage(image: Image, degrees: Int): Image = {
    val imageView = new ImageView(image)
    imageView.rotate = degrees
    val snapshotParams = new SnapshotParameters
    snapshotParams.fill = Transparent
    // scalastyle:off null
    imageView.snapshot(snapshotParams, null)
    // scalastyle:on null
  }

  def colorToWebString(color: Color): String = {
    f"#${(color.red * 255).toInt}%02X${(color.green * 255).toInt}%02X${(color.blue * 255).toInt}%02X"
  }
}
