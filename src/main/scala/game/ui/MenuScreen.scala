package game.ui

import scalafx.Includes._
import scalafx.scene.layout.VBox
import scalafx.scene.layout.Priority
import scalafx.scene.control.Button
import scalafx.scene.text._
import scalafx.scene.paint.Color._
import scalafx.geometry.Pos

object MenuResult extends Enumeration {
  val NewGame, Quit = Value
}

class MenuScreen(callback: (MenuResult.Value) => Unit) extends BaseScreen {

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
  title.text = "Land of Trieath"
  title.font = titleFont
  title.fill = White

  val newGame = new Button("New Game")
  newGame.font = buttonFont
  newGame.maxWidth = MenuScreen.ButtonWidth
  newGame.onAction = () => callback(MenuResult.NewGame)

  val exit = new Button("Exit")
  exit.font = buttonFont
  exit.maxWidth = MenuScreen.ButtonWidth
  exit.onAction = () => callback(MenuResult.Quit)

  layout.children = Seq(title, newGame, exit)

  content = layout
}

object MenuScreen {
  val Padding = 60
  val ButtonWidth = 300
  val StartWidth = 800
  val StartHeight = 600
  val TitleFontSize = 36
  val ButtonFontSize = 20
}
