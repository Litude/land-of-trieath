package game

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage

object UserInterface extends JFXApp {

  stage = new PrimaryStage {
    title = "Land of Trieath"
  }
  
  var activeScreen: BaseScreen = new GameScreen
  stage.scene = activeScreen
  
  stage.width.onChange {
    activeScreen.onResize()
  }
  stage.height.onChange {
    activeScreen.onResize()
  }
  
  def changeScreen(screen: BaseScreen): Unit = {
    activeScreen = screen
    stage.scene = screen
  }
  
}
