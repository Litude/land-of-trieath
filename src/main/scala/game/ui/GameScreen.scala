package game.ui

import scala.collection.mutable.ArrayBuffer

import scalafx.Includes._
import scalafx.animation._
import scalafx.beans.property._
import scalafx.scene.canvas._
import scalafx.scene.image._
import scalafx.scene.input._
import scalafx.scene.layout._
import scalafx.scene.paint.Color._
import scalafx.util.Duration

import game.core._

object GameResult extends Enumeration {
  val Victory, Defeat, Quit = Value
}

class GameScreen(game: Game, callback: (GameResult.Value, ArrayBuffer[Character]) => Unit) extends BaseScreen {

  var selectedCharacter: Option[Character] = None
  var hoveredTile = Coordinate(0 ,0)

  var _camOffset = Coordinate(0, 0)
  var centerPosition = Coordinate(0, 0)
  var mousePosition = Coordinate(0, 0)
  val tileEffects = new TileEffects()

  var showDebugInfo = false

  fill = Black

  def camOffset_=(offset: Coordinate): Unit = {
    _camOffset = offset
    clipCameraToBounds()
    translateScene()
  }

  def camOffset: Coordinate = _camOffset

  game.onDamageCaused = onDamageCaused
  game.onTurnEnded = () => onTurnEnded

  val cameraOffsets = Array.tabulate(game.playerList.length)(
    game.playerList(_).characters.headOption.map(cameraOffsetToCharacter).getOrElse(Coordinate(0, 0))
  )

  val backgroundCanvas = new Canvas(game.map.width * Tile.Size, game.map.height * Tile.Size)
  val foregroundCanvas = new Canvas(game.map.width * Tile.Size, game.map.height * Tile.Size)
  val overlay = new Overlay
  overlay.minWidth.bind(width - GameScreen.MenuWidth)
  overlay.minHeight.bind(height)
  overlay.visible = false
  drawGameMap(backgroundCanvas)

  val mapPane = new Pane
  mapPane.children = Array(backgroundCanvas, foregroundCanvas, overlay)
  mapPane.hgrow = Priority.Always

  val sideBar = new GameSideBar(
    GameScreen.MenuWidth,
    stopCharacterMovement(),
    selectNextPlayerCharacter(),
    skipSelectedCharacterTurn(),
    endTurn(),
    onResult(GameResult.Quit)
  )

  val layout = new HBox
  layout.children = Array(mapPane, sideBar)
  content = layout
  layout.minWidth.bind(width)

  val gameLoop = new Timeline {
    keyFrames = Seq(
      KeyFrame(Duration(GameScreen.TickDelay), onFinished = () => {
        game.updateGameState()
        deselectDeadCharacter()
        tileEffects.update(GameScreen.TickDelay)
        updateReachableTiles()
        sideBar.update(game, selectedCharacter, selectedCharacter.map(game.characterPlayer))
        drawGame(foregroundCanvas)
        isGameOver()
      })
    )
    cycleCount = Timeline.Indefinite
  }
  gameLoop.play()
  onTurnEnded()

  mapPane.onMouseClicked = mouseEvent => {
    if (backgroundCanvas.contains(backgroundCanvas.parentToLocal(mouseEvent.sceneX, mouseEvent.sceneY))) {
      val worldCoords = backgroundCanvas.parentToLocal(mouseEvent.sceneX, mouseEvent.sceneY)
      val tileCoords = Coordinate(worldCoords.x.toInt, worldCoords.y.toInt).worldToTile

      mouseEvent.button match {
        case MouseButton.Primary => {
          if (!overlay.visible.get) {
            selectedCharacter = game.playerList.flatMap(_.characters).find(_.occupiesPoint(Coordinate(worldCoords.x.toInt, worldCoords.y.toInt)))
          }
        }
        case MouseButton.Secondary if game.currentPlayerType == PlayerType.Human => {
          selectedCharacter.foreach(character => {
            if (game.moveCharacter(character, tileCoords) == MovementResult.Attacking) {
              this.tileEffects += new HighlightEffect(tileCoords)
            }
          })
        }
        case _ =>
      }
    }
  }

  mapPane.onMousePressed = mouseEvent => {
    if (mouseEvent.button == MouseButton.Middle) {
      mousePosition = Coordinate(mouseEvent.sceneX.toInt, mouseEvent.sceneY.toInt)
    }
  }

  mapPane.onMouseDragged = mouseEvent => {
    if (mouseEvent.button == MouseButton.Middle) {
      camOffset = Coordinate(camOffset.x + (mouseEvent.sceneX.toInt - mousePosition.x), camOffset.y + (mouseEvent.sceneY.toInt - mousePosition.y))
      mousePosition = Coordinate(mouseEvent.sceneX.toInt, mouseEvent.sceneY.toInt)
    }
  }

  onKeyPressed = keyEvent => {
    keyEvent.code match {
      case KeyCode.Left => camOffset = Coordinate(camOffset.x + GameScreen.ScrollSpeed, camOffset.y)
      case KeyCode.Right => camOffset = Coordinate(camOffset.x - GameScreen.ScrollSpeed, camOffset.y)
      case KeyCode.Up => camOffset = Coordinate(camOffset.x, camOffset.y + GameScreen.ScrollSpeed)
      case KeyCode.Down => camOffset = Coordinate(camOffset.x, camOffset.y - GameScreen.ScrollSpeed)
      case KeyCode.S => stopCharacterMovement()
      case KeyCode.D => showDebugInfo = !showDebugInfo
      case KeyCode.Tab => selectNextPlayerCharacter()
      case KeyCode.T => game.map.writeToFile("maps/temp.lvl")
      case _ =>
    }
  }

  mapPane.onMouseMoved = mouseEvent => {
    val mapPoint = backgroundCanvas.parentToLocal(mouseEvent.sceneX, mouseEvent.sceneY)
    hoveredTile = Coordinate(mapPoint.x.toInt, mapPoint.y.toInt).worldToTile
  }

  def selectNextPlayerCharacter(): Unit = {
    if (!game.isPaused && game.currentPlayerType == PlayerType.Human) {
      val characterList = game.playerList(game.currentPlayer).characters
      selectedCharacter = selectedCharacter.map(characterList.indexOf).getOrElse(-1) match {
        case -1 => characterList.filter(_.movementPoints > 0).headOption
        // move the currently selected character to the end of the list and pick the first character with points remaining
        case i => (characterList.drop(i + 1) ++ characterList.take(i + 1)).filter(_.movementPoints > 0).headOption
      }
    }
  }

  def stopCharacterMovement(): Unit = {
    if (!game.isPaused && game.currentPlayerType == PlayerType.Human) {
      selectedCharacter.foreach(character => {
        if (game.characterPlayer(character) == game.currentPlayer) {
          character.clearPath()
        }
      })
    }
  }

  def skipSelectedCharacterTurn(): Unit = {
    if (!game.isPaused && game.currentPlayerType == PlayerType.Human) {
      selectedCharacter.foreach(character => {
        if (game.characterPlayer(character) == game.currentPlayer) {
          character.endTurn()
          selectNextPlayerCharacter()
        }
      })
    }
  }

  def endTurn(): Unit = {
    if (game.currentPlayerType == PlayerType.Human) {
      cameraOffsets(game.currentPlayer) = camOffset
    }
    game.endTurn()
  }


  def onDamageCaused(damage: Int, position: Coordinate): Unit = {
    this.tileEffects += new DamageCounter(damage, position)
  }

  def onTurnEnded(): Unit = {
    selectedCharacter = None
    game.isPaused = true
    showOverlay(f"Player ${game.currentPlayer + 1} Turn", {
      if (game.currentPlayerType == PlayerType.Human) {
        camOffset = cameraOffsets(game.currentPlayer)
      } else {
        camOffset = game.playerList(game.currentPlayer).characters.headOption.map(cameraOffsetToCharacter).getOrElse(Coordinate(0, 0))
      }
    })
  }

  def cameraOffsetToCharacter(character: Character): Coordinate = {
    Coordinate(
      (character.position.x - game.map.width / 2) * Tile.Size * -1,
      (character.position.y - game.map.height / 2) * Tile.Size * -1
    )
  }

  def deselectDeadCharacter(): Unit = {
    selectedCharacter = if (selectedCharacter.map(_.isDead).getOrElse(false)) None else selectedCharacter
  }

  def updateReachableTiles(): Unit = {
    if (game.currentPlayerType == PlayerType.Human) {
      selectedCharacter.foreach(character => {
        game.updateReachableCharacterTiles(character)
      })
    }
  }

  def isGameOver(): Unit = {
    game.isOver match {
      case Some(victor) if (victor == GameScreen.LocalPlayer) => showOverlay("Victory!", "You have defeated the enemy", onResult(GameResult.Victory))
      case Some(victor) => showOverlay("Defeat!", "You have been defeated", onResult(GameResult.Defeat))
      case _ =>
    }
  }


  def clipCameraToBounds(): Unit = {
    _camOffset = Coordinate(
      Math.max(Math.min(_camOffset.x, -centerPosition.x), centerPosition.x - GameScreen.MenuWidth),
      Math.max(Math.min(_camOffset.y, -centerPosition.y), centerPosition.y)
      )
    if (width.get.toInt - GameScreen.MenuWidth > game.map.width * Tile.Size) {
      _camOffset = Coordinate(-GameScreen.MenuWidth / 2, _camOffset.y)
    }
    if (height.get.toInt > game.map.height * Tile.Size) {
      _camOffset = Coordinate(_camOffset.x, 0)
    }
  }

  override def onResize(): Unit = {
    centerPosition = Coordinate((width.get.toInt - game.map.width * Tile.Size) / 2, (height.get.toInt - game.map.height * Tile.Size) / 2)
    clipCameraToBounds()
    translateScene()
    mapPane.setMinWidth(width.get - GameScreen.MenuWidth)
    mapPane.setMaxWidth(width.get - GameScreen.MenuWidth)
  }

  def onResult(result: GameResult.Value): Unit = {
    gameLoop.stop()
    callback(result, game.playerList(game.currentPlayer).characters)
  }

  def translateScene(): Unit = {
    Array(backgroundCanvas, foregroundCanvas).foreach(canvas => {
      canvas.translateX = camOffset.x + centerPosition.x
      canvas.translateY = camOffset.y + centerPosition.y
    })
  }

  def drawGame(canvas: Canvas): Unit = {
    clearCanvas(canvas)
    if (showDebugInfo) drawDebugInfo(canvas)
    highlightHoveredTile(canvas)
    drawReachableTiles(canvas)
    drawGameCharacters(canvas)
    drawSelectedCharacterDetails(canvas)
    drawProjectiles(canvas)
    tileEffects.draw(canvas)
  }

  def clearCanvas(canvas: Canvas): Unit = {
    val context = canvas.graphicsContext2D
    context.clearRect(0, 0, canvas.width.get, canvas.height.get)
  }

  def highlightHoveredTile(canvas: Canvas): Unit = {
    highlightTiles(canvas, Seq(hoveredTile))
  }

  def drawReachableTiles(canvas: Canvas): Unit = {
    if (game.currentPlayerType == PlayerType.Human) {
      selectedCharacter.foreach(character => {
        highlightTiles(canvas, character.reachableTiles)
      })
    }
  }

  def highlightTiles(canvas: Canvas, tiles: Seq[Coordinate]): Unit = {
    val context = canvas.graphicsContext2D
    context.save()
    context.fill = Black
    context.globalAlpha = 0.25
    tiles.foreach(tile => {
      context.fillRect(tile.x * Tile.Size, tile.y * Tile.Size, Tile.Size, Tile.Size)
    })
    context.restore()
  }

  def drawGameMap(canvas: Canvas): Unit = {

    def drawTileOnCanvas(context: GraphicsContext, image: Image, index: Int, x: Int, y: Int): Unit = {
      context.drawImage(image,
        (index % GameScreen.TileMapWidth) * Tile.Size, (index / GameScreen.TileMapWidth) * Tile.Size, Tile.Size, Tile.Size,
        x * Tile.Size, y * Tile.Size, Tile.Size, Tile.Size
        )
    }

    val context = canvas.graphicsContext2D
    val groundTiles = new Image("file:img/tiles_ground.png")
    val obstacleTiles = new Image("file:img/tiles_obstacles.png")
    for {
      x <- 0 until game.map.width
      y <- 0 until game.map.height
    } {
      val tile = game.map(x, y)
      drawTileOnCanvas(context, groundTiles, tile.groundType, x, y)
      tile.obstacleType.foreach(drawTileOnCanvas(context, obstacleTiles, _, x, y))
    }
  }

  def drawSelectedCharacterDetails(canvas: Canvas): Unit = {
    val context = canvas.graphicsContext2D
    context.save()
    selectedCharacter.foreach(character => {
      val player = game.playerList.indexOf(game.playerList.find(_.characters.exists(_ == character)).get)
      context.stroke = GameScreen.PlayerColors(player)
      val position = character.drawingPosition
      context.strokeRect(position.x, position.y, Tile.Size, Tile.Size)

      //draw hitpoints bar
      context.stroke = White
      context.lineWidth = 1
      context.fill = Red
      context.strokeRect(position.x + 1, position.y - GameScreen.CharacterHitpointsHeight - 2, Tile.Size - 2, GameScreen.CharacterHitpointsHeight)
      context.fillRect(position.x + 1, position.y - GameScreen.CharacterHitpointsHeight - 2, Tile.Size - 2, GameScreen.CharacterHitpointsHeight)

      val hitpointsFraction = character.hitpoints.toDouble / character.maxHitPoints
      context.fill = Green
      context.fillRect(
        position.x + 1, position.y - GameScreen.CharacterHitpointsHeight - 2, (Tile.Size - 2) * hitpointsFraction, GameScreen.CharacterHitpointsHeight
      )
    })
    context.restore()
  }

  def drawGameCharacters(canvas: Canvas): Unit = {
    val context = canvas.graphicsContext2D
    context.save()
    game.playerList.flatMap(_.characters).foreach(character => {
      val position = character.drawingPosition
      val characterPlayer = game.characterPlayer(character)

      //draw glow below current human player characters with remaining movement points
      if (!character.isMoving && game.currentPlayerType == PlayerType.Human && characterPlayer == game.currentPlayer && character.movementPoints > 0) {
        val timeValue = (System.currentTimeMillis % GameScreen.MovementRemainingPeriod).toInt
        val multiplier = if (timeValue < GameScreen.MovementRemainingPeriod / 2) {
          GameScreen.MovementRemainingPeriod / 2 - timeValue
        } else {
          timeValue - GameScreen.MovementRemainingPeriod / 2
        }
        context.globalAlpha = 0.6 * multiplier / (GameScreen.MovementRemainingPeriod / 2)
        context.lineWidth = 1
        context.stroke = Gold
        context.strokeRect(position.x, position.y, Tile.Size, Tile.Size)
        context.globalAlpha = 1.0
      }
      context.drawImage(GameScreen.CharacterImageMap(character.characterClass.name),
        character.frame * Tile.Size, character.direction.id * GameScreen.CharacterHeight, Tile.Size, GameScreen.CharacterHeight,
        position.x, position.y, Tile.Size, Tile.Size
        )
      //draw a player colored badge in the upper-right corner
      GameScreen.PlayerColors.lift(characterPlayer).foreach(color => {
        context.fill = color
        context.stroke = White
        context.lineWidth = 1
        context.strokeRect(position.x + GameScreen.PlayerBadgeOffset, position.y + 1, GameScreen.PlayerBadgeSize, GameScreen.PlayerBadgeSize)
        context.fillRect(position.x + GameScreen.PlayerBadgeOffset, position.y + 1, GameScreen.PlayerBadgeSize, GameScreen.PlayerBadgeSize)
      })
    })
    context.restore()
  }

  def drawDebugInfo(canvas: Canvas): Unit = {
    val context = canvas.graphicsContext2D
    context.save()
    context.fill = Blue
    for {
      x <- 0 until game.map.width
      y <- 0 until game.map.height
    } {
      if (game.map.corridors.exists(_ == Coordinate(x, y))) {
        context.globalAlpha = 0.2
        context.fillRect(x * Tile.Size, y * Tile.Size, Tile.Size, Tile.Size)
        context.globalAlpha = 1
      }
    }
    context.restore()
  }

  def drawProjectiles(canvas: Canvas): Unit = {
    val context = canvas.getGraphicsContext2D
    game.projectiles.foreach(projectile => {
      GameScreen.CharacterProjectileMap.get(projectile.attacker.characterClass.toString).foreach(projectileImage => {
        val rotatedImage = UIUtils.rotateImage(projectileImage, projectile.angle)
        context.drawImage(rotatedImage, projectile.position.x, projectile.position.y)
      })
    })
  }

  def showOverlay(title: String, action: => Unit): Unit = {
    showOverlay(title, "", action)
  }

  def showOverlay(title: String, subtitle: String, action: => Unit): Unit = {
    game.isPaused = true
    overlay.show(title, subtitle, {
      action
      game.isPaused = false
      overlay.dismiss()
    })
  }
}

object GameScreen {
  val LocalPlayer = 0
  val CharacterImageMap = (CharacterClass.Values.map(
    characterClass => characterClass.name -> new Image(f"file:img/${characterClass.image}"))
  ).toMap

  val CharacterProjectileMap = (CharacterClass.Values.collect {
    case key if key.projectile.isDefined => key.name -> new Image(f"file:img/${key.projectile.get}")
  }).toMap

  val PlayerColors = Array(Blue, Red, Green, Yellow)

  val CharacterHeight = 36
  val CharacterWidth = 32
  val TileMapWidth = 8
  val ScrollSpeed = 16
  val TickDelay = 17 //milliseconds
  val MenuWidth = 200

  val PlayerBadgeOffset = 27
  val PlayerBadgeSize = 4
  val CharacterHitpointsHeight = 4

  val MovementRemainingPeriod = 2000

}
