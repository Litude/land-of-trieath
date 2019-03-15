package game

import scalafx.Includes._
import scalafx.scene.layout.VBox
import scalafx.scene.text.{Text, TextAlignment}
import scalafx.scene.shape.Rectangle
import scalafx.scene.control.Button
import scalafx.scene.paint.Color._
import scalafx.scene.layout.Priority

class GameSideBar(width: Int, endTurn: () => Unit) extends VBox {
  
  val menuSpacer = Rectangle(width, 1, Black)
  val buttonEndTurn = new Button("End Turn")
  buttonEndTurn.onAction = endTurn
  
  val textCurrentPlayer = new Text
  textCurrentPlayer.text = ""
  textCurrentPlayer.fill = White
  
  val textCharacterHP = new Text
  textCharacterHP.text = ""
  textCharacterHP.fill = White
  textCharacterHP.visible = false
  
  val textCharacterMovement = new Text
  textCharacterMovement.text = ""
  textCharacterMovement.fill = White
  textCharacterMovement.visible = false
  
  val textCharacterClass = new Text
  textCharacterClass.text = ""
  textCharacterClass.fill = White
  textCharacterClass.visible = false
  
  val textCharacterOwner = new Text
  textCharacterOwner.text = ""
  textCharacterOwner.fill = White
  textCharacterOwner.visible = false
  
  this.children = Array(menuSpacer, buttonEndTurn, textCurrentPlayer, textCharacterHP, textCharacterMovement, textCharacterClass, textCharacterOwner)
  this.hgrow = Priority.Never
  
  def updateCurrentPlayerText(currentPlayer: Int): Unit = {
    textCurrentPlayer.text = f"Current Turn: Player ${currentPlayer + 1}"
  }
  
  def updateCharacterUI(selectedCharacter: Option[Character], characterPlayer: Option[Int]): Unit = {
    (selectedCharacter, characterPlayer) match {
      case (Some(character), Some(player)) => {
        textCharacterHP.text = f"Hitpoints: ${character.hitpoints}/${character.maxHitPoints}"
        textCharacterHP.visible = true
        textCharacterMovement.text = f"Movement: ${character.movementPoints}/${character.maxMovementPoints}"
        textCharacterMovement.visible = true
        textCharacterClass.text = f"Class: ${character.charType}"
        textCharacterClass.visible = true
        textCharacterOwner.text = f"Owner: Player ${player + 1}"
        textCharacterOwner.visible = true
      }
      case _ => {
        textCharacterHP.visible = false
        textCharacterMovement.visible = false
        textCharacterClass.visible = false
        textCharacterOwner.visible = false
      }
    }
  }
  
}