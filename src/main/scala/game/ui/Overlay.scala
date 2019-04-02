package game.ui

import scalafx.Includes._
import scalafx.geometry._
import scalafx.scene.control.Button
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.text._

class Overlay extends Pane {
  val overlayColor = Color(0, 0, 0, Overlay.Opacity)
  background = new Background(Array(new BackgroundFill(overlayColor, CornerRadii.Empty, Insets.Empty)))

  val layout = new VBox(Overlay.LayoutSpacing)
  layout.minWidth.bind(minWidth)
  layout.minHeight.bind(minHeight)
  layout.alignment = Pos.Center

  val title = new Text
  title.font = Font("Verdana", Overlay.TitleSize)
  title.fill = White

  val subtitle = new Text
  subtitle.font = Font("Verdana", Overlay.SubtitleSize)
  subtitle.fill = White

  val button = new Button("Continue")

  layout.children = Seq(title, subtitle, button)
  children = layout

  def show(titleText: String, subtitleText: String, action: => Unit): Unit = {
    visible = true
    title.text = titleText
    subtitle.text = subtitleText
    button.onAction = () => action
    button.requestFocus()
  }

  def dismiss(): Unit = {
    visible = false
  }
}

object Overlay {
  val TitleSize = 36
  val SubtitleSize = 16
  val LayoutSpacing = 20
  val Opacity = 0.75
}
