package game.ui

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.image._
import scalafx.scene.paint.Color._
import scalafx.scene.SnapshotParameters
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

  def rotateImage(image: Image, degrees: Int): Image = {
    val imageView = new ImageView(image)
    imageView.rotate = degrees
    val snapshotParams = new SnapshotParameters
    snapshotParams.fill = Transparent
    // scalastyle:off null
    imageView.snapshot(snapshotParams, null)
    // scalastyle:on null
  }
}
