package game.ui

import scala.collection.mutable.ArrayBuffer

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.stage.Stage

import game.core.Character
import game.core.GameMaster

object UserInterface extends JFXApp {

  stage = new PrimaryStage
  stage.title = "Land of Trieath"

  var activeStage: Stage = stage
  var activeScreen: BaseScreen = new MenuScreen(handleMenuResult)
  changeScreen(stage, new MenuScreen(handleMenuResult))

  // change stages when moving between the game view any any menu view since otherwise the window won't get resized properly
  def changeStage(screen: BaseScreen): Unit = {
    val newStage = new Stage
    newStage.title = "Land of Trieath"
    changeScreen(newStage, screen)
    newStage.show
    activeStage.close
    activeStage = newStage
  }

  def changeScreen(screen: BaseScreen): Unit = {
    // change screen when moving between menu screens
    changeScreen(activeStage, screen)
  }

  def changeScreen(currentStage: Stage, screen: BaseScreen): Unit = {
    activeScreen = screen
    currentStage.scene = screen

    currentStage.width.onChange {
      activeScreen.onResize()
    }
    currentStage.height.onChange {
      activeScreen.onResize()
    }
  }

  def handleMenuResult(result: MenuResult.Value): Unit = {
    result match {
      case MenuResult.NewGame => {
        GameMaster.readCampaignFromFile("default.json")
        changeScreen(new CampaignScreen(handleCampaignResult))
      }
      case MenuResult.Quit => activeStage.close
    }
  }

  def handleCampaignResult(result: CampaignResult.Value): Unit = {
    result match {
      case CampaignResult.Back => changeScreen(new MenuScreen(handleMenuResult))
      case CampaignResult.Start =>  GameMaster.createGame.foreach(game => changeStage(new GameScreen(game, handleGameResult)))
    }
  }

  def handleGameResult(result: GameResult.Value, playerCharacters: ArrayBuffer[Character]): Unit = {
    result match {
      case GameResult.Defeat => changeStage(new CampaignScreen(handleCampaignResult))
      case GameResult.Victory => {
        GameMaster.missionWon(playerCharacters)
        changeStage(new CampaignScreen(handleCampaignResult))
      }
      case GameResult.Quit => changeStage(new MenuScreen(handleMenuResult))
    }
  }

}
