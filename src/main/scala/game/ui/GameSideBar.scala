package game.ui

import scalafx.Includes._
import scalafx.beans.property._
import scalafx.geometry._
import scalafx.geometry.Rectangle2D
import scalafx.scene.control.Button
import scalafx.scene.image._
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import scalafx.scene.text._
import scalafx.scene.control.ProgressBar

import game.core.Character
import game.core.Human
import game.core.Game
import game.core.Direction

class GameSideBar(width: Int, stop: () => Unit, next: () => Unit, skip: () => Unit, endTurn: () => Unit) extends VBox(GameSideBar.DefaultPadding) {

  background = new Background(Array(new BackgroundFill(Black, CornerRadii.Empty, Insets.Empty)))
  val menuSpacer = Rectangle(width, 1, Black)

  alignment = Pos.TopCenter

  stylesheets = Seq("styles.css")

  val textCurrentPlayer = new Text
  textCurrentPlayer.text = ""
  textCurrentPlayer.fill = White
  textCurrentPlayer.font = Font("Verdana", 16)
  textCurrentPlayer.textAlignment = TextAlignment.Center

  val topSpacer = new Region
  topSpacer.prefHeight = 20

  val characterInfoTopSpacer = new Region
  characterInfoTopSpacer.prefHeight = 40

  val characterOverview = new CharacterOverview

  val overviewSpacer = new Region
  overviewSpacer.prefHeight = 10

  val hitpointsBar = new CharacterBar("Hitpoints", Green, Red, this.width - 50)
  val movementBar = new CharacterBar("Movement", Green, Blue, this.width - 50)

  val statsSpacer = new Region
  statsSpacer.prefHeight = 6

  val characterStats = new CharacterStats(this.width - 50)

  val buttonsSpacer = new Region
  buttonsSpacer.prefHeight = 10

  val buttonGrid = new ButtonGrid(width)

  children = Seq(
    menuSpacer,
    topSpacer,
    textCurrentPlayer,
    characterInfoTopSpacer,
    characterOverview,
    overviewSpacer,
    hitpointsBar,
    movementBar,
    statsSpacer,
    characterStats,
    buttonsSpacer,
    buttonGrid
  )

  hgrow = Priority.Never

  def updateCurrentPlayerText(currentPlayer: Int): Unit = {
    textCurrentPlayer.text = f"Current Turn:\nPlayer ${currentPlayer + 1}"
  }

  def updateCharacterUI(game: Game, selectedCharacter: Option[Character], characterPlayer: Option[Int]): Unit = {
    buttonGrid.update(selectedCharacter, game)
    (selectedCharacter, characterPlayer) match {
      case (Some(character), Some(player)) => {
        characterOverview.update(character, player)
        characterOverview.visible = true
        hitpointsBar.update(character.hitpoints, character.maxHitPoints)
        hitpointsBar.visible = true
        movementBar.update(character.movementPoints, character.maxMovementPoints)
        movementBar.visible = true
        characterStats.update(character)
        characterStats.visible = true
      }
      case _ => {
        characterOverview.visible = false
        hitpointsBar.visible = false
        movementBar.visible = false
        characterStats.visible = false
      }
    }
  }

  class CharacterOverview extends GridPane {
    val characterImage = new ImageView
    characterImage.viewport = new Rectangle2D(
      GameScreen.CharacterWidth,
      GameScreen.CharacterHeight * Direction.South.id,
      GameScreen.CharacterWidth,
      GameScreen.CharacterHeight
    )

    val characterStack = new StackPane
    characterStack.children = characterImage

    val textCharacterClass = new Text
    textCharacterClass.fill = White

    val textCharacterTeam = new Text
    textCharacterTeam.fill = White

    add(characterStack, 0, 0, 1, 2)
    add(textCharacterTeam, 1, 0)
    add(textCharacterClass, 1, 1)
    alignment = Pos.Center
    hgap = 10.0
    vgap = 4.0

    val col1 = new ColumnConstraints
    col1.minWidth = 38
    col1.fillWidth = true
    col1.halignment = HPos.Center
    columnConstraints = Seq(col1)

    def update(character: Character, player: Int): Unit = {
      characterImage.image = GameScreen.CharacterImageMap(character.charType)
      characterStack.style = f"-fx-border-color: ${UIUtils.colorToWebString(GameScreen.PlayerColors(player))}"
      textCharacterClass.text = f"Class: ${character.charType}"
      textCharacterTeam.text = f"Team: Player ${player + 1}"
    }
  }

  class CharacterBar(val label: String, barColor: Color, backgroundColor: Color, val displayWidth: Int) extends VBox(GameSideBar.DefaultPadding) {
    alignment = Pos.TopCenter

    val barLabel = new Text
    barLabel.text = f"${label}: 0/0"
    barLabel.fill = White

    val bar = new ProgressBar
    bar.progress = 0
    bar.style = f"-fx-control-inner-background: ${UIUtils.colorToWebString(backgroundColor)}; -fx-accent: ${UIUtils.colorToWebString(barColor)};"
    bar.maxWidth = displayWidth

    children = Seq(barLabel, bar)

    def update(currentValue: Int, maxValue: Int): Unit = {
      barLabel.text = f"${label}: ${currentValue}/${maxValue}"
      bar.progress = currentValue.toDouble / maxValue
    }
  }

  class CharacterStats(val displayWidth: Int) extends GridPane {
    maxWidth = displayWidth

    val legendAttack = new Text
    legendAttack.fill = White
    legendAttack.text = "Attack:"
    val legendDefense = new Text
    legendDefense.fill = White
    legendDefense.text = "Defense:"
    val legendRange = new Text
    legendRange.fill = White
    legendRange.text = "Range:"

    val valueAttack = new Text
    valueAttack.fill = White
    val valueDefense = new Text
    valueDefense.fill = White
    val valueRange = new Text
    valueRange.fill = White

    add(legendAttack, 0, 0)
    add(legendDefense, 0, 1)
    add(legendRange, 0, 2)

    add(valueAttack, 1, 0)
    add(valueDefense, 1, 1)
    add(valueRange, 1, 2)

    vgap = 4.0

    val col1 = new ColumnConstraints
    col1.halignment = HPos.Left
    col1.minWidth = displayWidth / 2

    val col2 = new ColumnConstraints
    col2.halignment = HPos.Right
    col2.minWidth = displayWidth / 2

    columnConstraints = Seq(col1, col2)

    def update(character: Character): Unit = {
      valueAttack.text = f"${character.attackPower}"
      valueDefense.text = f"${character.defensePower}"
      valueRange.text = f"${character.range}"
    }
  }

  class ButtonGrid(parentWidth: ReadOnlyDoubleProperty) extends GridPane {
    val buttonStop = new Button("Stop")
    buttonStop.disable = true
    buttonStop.onAction = stop
    val buttonNext = new Button("Next")
    buttonNext.disable = true
    buttonNext.onAction = next
    val buttonSkip = new Button("Skip")
    buttonSkip.disable = true
    buttonSkip.onAction = skip
    val buttonEndTurn = new Button("End Turn")
    buttonEndTurn.disable = true
    buttonEndTurn.onAction = endTurn

    buttonStop.maxWidth = GameSideBar.ButtonWidth
    buttonNext.maxWidth = GameSideBar.ButtonWidth
    buttonSkip.maxWidth = GameSideBar.ButtonWidth
    buttonEndTurn.maxWidth = GameSideBar.ButtonWidth

    add(buttonStop, 0, 0)
    add(buttonNext, 1, 0)
    add(buttonSkip, 0, 1)
    add(buttonEndTurn, 1, 1)

    vgap = 16.0

    alignment = Pos.TopCenter

    val colSpecs = new ColumnConstraints
    colSpecs.fillWidth = true
    colSpecs.minWidth.bind(parentWidth / 2)
    colSpecs.halignment = HPos.Center

    columnConstraints = Seq(colSpecs, colSpecs)

    def update(selectedCharacter: Option[Character], game: Game): Unit = {
      if (game.currentPlayerType == Human) {
        buttonEndTurn.disable = !game.actionsAllowed
        selectedCharacter match {
          case Some(character) if (game.characterPlayer(character) == game.currentPlayer) => {
          buttonStop.disable = !character.isMoving
          buttonNext.disable = false
          buttonSkip.disable = character.movementPoints == 0 || character.isMoving
          }
          case _ => {
            buttonStop.disable = true
            buttonNext.disable = true
            buttonSkip.disable = true
          }
        }
      } else {
        buttonStop.disable = true
        buttonNext.disable = true
        buttonSkip.disable = true
        buttonEndTurn.disable = true
      }
    }
  }
}

object GameSideBar {
  val DefaultPadding = 4
  val ButtonWidth = 80
}
