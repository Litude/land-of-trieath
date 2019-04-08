package game.ui

import scala.collection.mutable.ArrayBuffer

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.stage.Stage

import game.core.Character
import game.core.Game
import game.core.GameMaster

object UserInterface extends JFXApp {

  stage = new PrimaryStage
  stage.title = "Land of Trieath"

  def menuScreen: MenuScreen = new MenuScreen(handleMenuResult)
  def campaignScreen: CampaignScreen = new CampaignScreen(handleCampaignResult)
  def intermissionScreen: IntermissionScreen = new IntermissionScreen(handleIntermissionResult)
  def gameScreen(game: Game): GameScreen = new GameScreen(game, handleGameResult)

  var activeStage: Stage = stage
  var activeScreen: BaseScreen = menuScreen
  changeScreen(stage, menuScreen)


  // change stages when moving between the game view any any menu view since otherwise the window won't get resized properly
  def changeStage(screen: BaseScreen): Unit = {
    val newStage = new Stage
    newStage.title = "Land of Trieath"
    changeScreen(newStage, screen)
    newStage.show
    activeStage.close
    activeStage = newStage
  }

  // change screen when moving between menu screens
  def changeScreen(screen: BaseScreen): Unit = {
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
      case MenuResult.NewGame => changeScreen(campaignScreen)
      case MenuResult.Quit => activeStage.close
    }
  }

  def handleCampaignResult(result: CampaignResult.Value, campaignFilename: Option[String]): Unit = {
    result match {
      case CampaignResult.Back => changeScreen(menuScreen)
      case CampaignResult.Selected => {
        campaignFilename.foreach(filename => {
          GameMaster.loadCampaign(filename)
          changeScreen(intermissionScreen)
        })
      }
    }
  }

  def handleIntermissionResult(result: IntermissionResult.Value): Unit = {
    result match {
      case IntermissionResult.Back => changeScreen(campaignScreen)
      case IntermissionResult.Start =>  GameMaster.createGame.foreach(game => changeStage(gameScreen(game)))
    }
  }

  def handleGameResult(result: GameResult.Value, playerCharacters: ArrayBuffer[Character]): Unit = {
    result match {
      case GameResult.Defeat => changeStage(intermissionScreen)
      case GameResult.Victory => {
        GameMaster.missionWon(playerCharacters)
        changeStage(intermissionScreen)
      }
      case GameResult.Quit => changeStage(menuScreen)
    }
  }

}
