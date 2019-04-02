package game.ui

import scalafx.scene.Scene

trait BaseScreen extends Scene {
  def onResize(): Unit = {}
}
