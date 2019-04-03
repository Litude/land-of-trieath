package game.ui

import scalafx.Includes._
import scalafx.geometry.Pos
import scalafx.scene.control.Button
import scalafx.scene.layout.HBox
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color._
import scalafx.scene.text._

import game.core.GameMaster

object CampaignResult extends Enumeration {
  val Start, Back = Value
}

class CampaignScreen(callback: (CampaignResult.Value) => Unit) extends BaseScreen {
  val layout = new VBox(MenuScreen.Padding)
  layout.minWidth.bind(width)
  layout.minHeight.bind(height)
  layout.prefWidth = MenuScreen.StartWidth
  layout.prefHeight = MenuScreen.StartHeight
  layout.alignment = Pos.Center
  fill = Black

  val titleFont = new Font("Verdana", MenuScreen.TitleFontSize)
  val buttonFont = new Font("Verdana", MenuScreen.ButtonFontSize)

  val title = new Text
  title.text = GameMaster.campaignName
  title.font = titleFont
  title.fill = White

  val progress = new Text
  progress.text = if (!GameMaster.isWon) f"Mission: ${GameMaster.mission + 1}" else "Campaign complete!"
  progress.font = titleFont
  progress.fill = White

  val buttonLayout = new HBox(MenuScreen.Padding)
  buttonLayout.minWidth.bind(width)
  buttonLayout.alignment = Pos.Center

  val start = new Button("Start Game!")
  start.font = buttonFont
  start.maxWidth = MenuScreen.ButtonWidth
  start.onAction = () => callback(CampaignResult.Start)
  start.disable = GameMaster.isWon

  val back = new Button("Back")
  back.font = buttonFont
  back.maxWidth = MenuScreen.ButtonWidth
  back.onAction = () => callback(CampaignResult.Back)

  buttonLayout.children = Seq(back, start)
  layout.children = Seq(title, progress, buttonLayout)

  content = layout
}
