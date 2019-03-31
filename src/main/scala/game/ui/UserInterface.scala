package game.ui

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.stage.Stage

object UserInterface extends JFXApp {

  stage = new PrimaryStage

  var activeStage: Stage = stage
  var activeScreen: BaseScreen = new MenuScreen(handleMenuResult)
  changeScreen(stage, new MenuScreen(handleMenuResult))

  def changeScreen(screen: BaseScreen): Unit = {
    val newStage = new Stage
    changeScreen(newStage, screen)
  }

  def changeScreen(currentStage: Stage, screen: BaseScreen): Unit = {
    currentStage.title = "Land of Trieath"
    activeScreen = screen
    currentStage.scene = screen

    currentStage.width.onChange {
      activeScreen.onResize()
    }
    currentStage.height.onChange {
      activeScreen.onResize()
    }

    currentStage.show
    activeStage.close
    activeStage = currentStage
  }

  def handleMenuResult(result: MenuResult.Value): Unit = {
    result match {
      case MenuResult.NewGame => changeScreen(new GameScreen(handleGameResult))
      case MenuResult.Quit => activeStage.close
    }
  }

  def handleGameResult(result: GameResult.Value): Unit = {
    result match {
      case _ => changeScreen(new MenuScreen(handleMenuResult))
    }
  }

}
