package game.ui

import java.io.File

import scala.util.{Failure, Success, Try}

import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Pos
import scalafx.scene.control.Button
import scalafx.scene.control.ListView
import scalafx.scene.layout.HBox
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color._
import scalafx.scene.text._

import game.core.Campaign

object CampaignResult extends Enumeration {
  val Selected, Back = Value
}

class CampaignScreen(callback: (CampaignResult.Value, Option[String]) => Unit) extends BaseScreen {
  var selectedItem: Option[CampaignItem] = None

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
  title.text = "Campaign Selection"
  title.font = titleFont
  title.fill = White

  val list = createListView

  val buttonLayout = new HBox(MenuScreen.Padding)
  buttonLayout.minWidth.bind(width)
  buttonLayout.alignment = Pos.Center

  val next = new Button("Next")
  next.font = buttonFont
  next.maxWidth = MenuScreen.ButtonWidth
  next.onAction = () => callback(CampaignResult.Selected, selectedItem.map(_.filename))
  next.disable = true

  val back = new Button("Back")
  back.font = buttonFont
  back.maxWidth = MenuScreen.ButtonWidth
  back.onAction = () => callback(CampaignResult.Back, None)

  buttonLayout.children = Seq(back, next)
  layout.children = Seq(title, list, buttonLayout)

  content = layout

  def createListView: ListView[CampaignItem] = {
    val list = new ListView[CampaignItem]
    list.items = createCampaignList
    list.selectionModel.get.selectedItemProperty.onChange((_, _, selection) => {
      selectedItem = Option(selection)
      next.disable = !selectedItem.isDefined
    })
    list.maxWidth.bind(width - CampaignScreen.ListSidePadding)
    list.maxHeight.bind(height - CampaignScreen.ListTopPadding)
    list
  }

  def createCampaignList: ObservableBuffer[CampaignItem] = {
    (Try(new File(Campaign.Directory)) match {
    case Success(file) if file.exists && file.isDirectory => file.listFiles.filter(_.isFile).toList
    case _ => List()
    })
    .map(file => Campaign.readFromFile(file.getName).map(campaign => CampaignItem(campaign.name, file.getName)))
    .flatten
    .sortBy(_.campaignName)
    .to[ObservableBuffer]
  }
}

object CampaignScreen {
  val ListSidePadding = 200
  val ListTopPadding = 400
}

case class CampaignItem(val campaignName: String, val filename: String) {
  override def toString: String = campaignName
}
