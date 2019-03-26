package game.ui

import scalafx.Includes._
import scalafx.animation._
import scalafx.beans.property._
import scalafx.geometry.Point2D
import scalafx.scene.canvas._
import scalafx.scene.image._
import scalafx.scene.input._
import scalafx.scene.layout._
import scalafx.scene.paint.Color._
import scalafx.util.Duration

import game.core._

class GameScreen extends BaseScreen {

  var selectedCharacter: Option[Character] = None
  var hoveredTile = Coordinate(0 ,0)

  var camOffset = Coordinate(0, 0)
  var centerPosition = Coordinate(0, 0)
  var mousePosition = Coordinate(0, 0)
  val tileEffects = new TileEffects()

  var showDebugInfo = false

  fill = Black

  //add some test data
  val testPlayer = new HumanPlayer("tester")
  testPlayer.characters ++= Seq(
    new Character(100, 20, Warrior, Direction.South),
    new Character(100, 20, Monk, Direction.South),
    new Character(100, 20, Ranger, Direction.South),
    new Character(100, 20, Monk, Direction.South),
    new Character(100, 20, Warrior, Direction.South),
    new Character(100, 20, Warrior, Direction.South),
    new Character(100, 20, Ranger, Direction.South)
  )

  val testPlayer2 = new AIPlayer("other", new BasicAI)
  testPlayer2.characters ++= Seq(
    new Character(30, 20, Monk, Direction.North),
    new Character(100, 20, Monk, Direction.North),
    new Character(100, 20, Warrior, Direction.North),
    new Character(100, 20, Monk, Direction.North),
    new Character(100, 20, Ranger, Direction.North),
    new Character(100, 20, Ranger, Direction.North),
    new Character(100, 20, Monk, Direction.North)
  )

  val game = new Game(Array(testPlayer, testPlayer2), onDamageCaused)

  val backgroundCanvas = new Canvas(game.map.width * Tile.Size, game.map.height * Tile.Size)
  val foregroundCanvas = new Canvas(game.map.width * Tile.Size, game.map.height * Tile.Size)
  drawGameMap(backgroundCanvas)

  val mapPane = new Pane
  mapPane.children = Array(backgroundCanvas, foregroundCanvas)
  mapPane.hgrow = Priority.Always

  val menu = new GameSideBar(
    GameScreen.MenuWidth,
    () => stopCharacterMovement(),
    () => selectNextPlayerCharacter(),
    () => skipSelectedCharacterTurn(),
    () => endTurn()
  )

  val layout = new HBox
  layout.children = Array(mapPane, menu)
  content = layout

  val gameLoop = new Timeline {
    keyFrames = Seq(
      KeyFrame(Duration(GameScreen.TickDelay), onFinished = () => {
        game.updateGameState()
        deselectDeadCharacter()
        menu.updateCurrentPlayerText(game.currentPlayer)
        menu.updateCharacterUI(game, selectedCharacter, selectedCharacter.map(game.characterPlayer))
        clearCanvas(foregroundCanvas)
        if (showDebugInfo) drawDebugInfo(foregroundCanvas)
        highlightHoveredTile(foregroundCanvas)
        updateReachableTiles(foregroundCanvas)
        drawGameCharacters(foregroundCanvas)
        drawSelectedCharacterDetails(foregroundCanvas)
        drawProjectiles(foregroundCanvas)
        drawScreenEffects(foregroundCanvas, GameScreen.TickDelay)
      })
    )
    cycleCount = Timeline.Indefinite
  }
  gameLoop.play()

  mapPane.onMouseClicked = mouseEvent => {
    if (backgroundCanvas.contains(backgroundCanvas.parentToLocal(mouseEvent.sceneX, mouseEvent.sceneY))) {
      val worldCoords = backgroundCanvas.parentToLocal(mouseEvent.sceneX, mouseEvent.sceneY)
      val tileCoords = worldToTile(worldCoords)

      mouseEvent.button match {
        case MouseButton.Primary => {
          selectedCharacter = game.playerList.flatMap(_.characters).find(_.occupiesPoint(Coordinate(worldCoords.x.toInt, worldCoords.y.toInt)))
        }
        case MouseButton.Secondary if game.currentPlayerType == Human => {
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
      clipCameraToBounds()
      translateScene()
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
      case _ =>
    }
    clipCameraToBounds()
    translateScene()
  }

  mapPane.onMouseMoved = mouseEvent => {
    val mapPoint = backgroundCanvas.parentToLocal(mouseEvent.sceneX, mouseEvent.sceneY)
    hoveredTile = worldToTile(Coordinate(mapPoint.x.toInt, mapPoint.y.toInt))
  }

  def selectNextPlayerCharacter(): Unit = {
    val characterList = game.playerList(game.currentPlayer).characters
    selectedCharacter = selectedCharacter.map(characterList.indexOf).getOrElse(-1) match {
      case -1 => characterList.headOption
      case i => characterList.lift((i + 1) % characterList.length)
    }
  }

  def stopCharacterMovement(): Unit = {
    selectedCharacter.foreach(character => {
      if (game.characterPlayer(character) == game.currentPlayer) {
      character.clearPath()
      }
    })
  }

  def skipSelectedCharacterTurn(): Unit = {
    selectedCharacter.foreach(character => {
      if (game.characterPlayer(character) == game.currentPlayer) {
      character.endTurn()
      }
    })
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

  def endTurn(): Unit = {
    if (game.endTurn()) {
      selectedCharacter = None
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
      if (!character.isMoving && game.currentPlayerType == Human && characterPlayer == game.currentPlayer && character.movementPoints > 0) {
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
      context.drawImage(GameScreen.CharacterImageMap(character.charType),
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

  def highlightHoveredTile(canvas: Canvas): Unit = {
    highlightTiles(canvas, Seq(hoveredTile))
  }

  def updateReachableTiles(canvas: Canvas): Unit = {
    if (game.currentPlayerType == Human) {
      selectedCharacter.foreach(character => {
        game.updateReachableCharacterTiles(character)
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

  def clearCanvas(canvas: Canvas): Unit = {
    val context = canvas.graphicsContext2D
    context.clearRect(0, 0, canvas.width.get, canvas.height.get)
  }

  def clipCameraToBounds(): Unit = {
    camOffset = Coordinate(
      Math.max(Math.min(camOffset.x, -centerPosition.x), centerPosition.x - GameScreen.MenuWidth),
      Math.max(Math.min(camOffset.y, -centerPosition.y), centerPosition.y)
      )
    if (width.get.toInt - GameScreen.MenuWidth > game.map.width * Tile.Size) {
      camOffset = Coordinate(-GameScreen.MenuWidth / 2, camOffset.y)
    }
    if (height.get.toInt > game.map.height * Tile.Size) {
      camOffset = Coordinate(camOffset.x, 0)
    }
  }

  def onResize(): Unit = {
    centerPosition = Coordinate(width.get.toInt / 2 - game.map.width / 2 * Tile.Size, height.get.toInt / 2 - game.map.height / 2 * Tile.Size)
    clipCameraToBounds()
    translateScene()
    mapPane.setMinWidth(width.get - GameScreen.MenuWidth)
    mapPane.setMaxWidth(width.get - GameScreen.MenuWidth)
  }

  def worldToTile(world: Point2D): Coordinate = worldToTile(Coordinate(world.x.toInt, world.y.toInt))

  def worldToTile(world: Coordinate): Coordinate = Coordinate(world.x / Tile.Size, world.y / Tile.Size)

  def translateScene(): Unit = {
    Array(backgroundCanvas, foregroundCanvas).foreach(canvas => {
      canvas.translateX = camOffset.x + centerPosition.x
      canvas.translateY = camOffset.y + centerPosition.y
    })
  }

  def onDamageCaused(damage: Int, position: Coordinate): Unit = {
    this.tileEffects += new DamageCounter(damage, position)
  }

  def drawScreenEffects(canvas: Canvas, delay: Int): Unit = {
    this.tileEffects.update(delay)
    this.tileEffects.draw(canvas)
  }

  def drawProjectiles(canvas: Canvas): Unit = {
    val context = canvas.getGraphicsContext2D
    game.projectiles.foreach(projectile => {
      val rotatedImage = UIUtils.rotateImage(GameScreen.ProjectileImage, projectile.angle)
      context.drawImage(rotatedImage, projectile.position.x, projectile.position.y)
    })
  }

  def deselectDeadCharacter(): Unit = {
    selectedCharacter = if (selectedCharacter.map(_.isDead).getOrElse(false)) None else selectedCharacter
  }
}

object GameScreen {
  val ProjectileImage = new Image("file:img/arrow.png")

  val CharacterImageMap = scala.collection.immutable.Map[CharacterType, Image](
    Warrior -> new Image("file:img/warrior.png"),
    Monk -> new Image("file:img/monk.png"),
    Ranger -> new Image("file:img/ranger.png")
    )

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
