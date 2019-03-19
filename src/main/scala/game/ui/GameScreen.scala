package game.ui

import scalafx.Includes._
import scalafx.animation._
import scalafx.beans.property._
import scalafx.geometry.Point2D
import scalafx.scene.canvas._
import scalafx.scene.image.Image
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

  fill = Black

  val game = new Game(onDamageCaused)

  val characterImageMap = scala.collection.immutable.Map(
      Warrior.toString -> new Image("file:img/warrior_m.png"),
      Monk.toString -> new Image("file:img/townfolk1_m.png")
      )

  val backgroundCanvas = new Canvas(game.map.width * Tile.Size, game.map.height * Tile.Size)
  val foregroundCanvas = new Canvas(game.map.width * Tile.Size, game.map.height * Tile.Size)
  drawGameMap(backgroundCanvas)

  val mapPane = new Pane
  mapPane.children = Array(backgroundCanvas, foregroundCanvas)
  mapPane.hgrow = Priority.Always

  val menu = new GameSideBar(GameScreen.MenuWidth, () => endTurn())

  val layout = new HBox
  layout.children = Array(mapPane, menu)
  content = layout

  //add some test data
  val testChar = new Character(100, 20, Warrior, Direction.South)
  val testPlayer = new HumanPlayer("tester")
  //testChar.moveTo(Coordinate(10, 0))
  testPlayer.characters += testChar
  val testChar2 = new Character(100, 20, Monk, Direction.South)
  testChar2.moveTo(Coordinate(6, 3))
  testPlayer.characters += testChar2
  game.playerList += testPlayer

  val testPlayer2 = new AIPlayer("other", new BasicAI)
  val otherChar1 = new Character(30, 20, Monk, Direction.North)
  otherChar1.moveTo(Coordinate(3, 6))
  testPlayer2.characters += otherChar1
  val otherChar2 = new Character(100, 20, Monk, Direction.North)
  otherChar2.moveTo(Coordinate(10, 20))
  testPlayer2.characters += otherChar2
  game.playerList += testPlayer2

  val gameLoop = new Timeline {
    keyFrames = Seq(
      KeyFrame(Duration(GameScreen.TickDelay), onFinished = () => {
        game.updateGameState()
        deselectDeadCharacter()
        menu.updateCurrentPlayerText(game.currentPlayer)
        menu.updateCharacterUI(selectedCharacter, selectedCharacter.map(game.characterPlayer))
        clearCanvas(foregroundCanvas)
        highlightHoveredTile(foregroundCanvas)
        updateReachableTiles(foregroundCanvas)
        drawSelectionOutline(foregroundCanvas)
        drawGameCharacters(foregroundCanvas)
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
          selectedCharacter = game.playerList.flatMap(_.characters).find(_.position == tileCoords)
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
        case _ =>
      }
    clipCameraToBounds()
    translateScene()
  }

  mapPane.onMouseMoved = mouseEvent => {
    val mapPoint = backgroundCanvas.parentToLocal(mouseEvent.sceneX, mouseEvent.sceneY)
    hoveredTile = worldToTile(Coordinate(mapPoint.x.toInt, mapPoint.y.toInt))
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
      y <- 0 until game.map.width
      x <- 0 until game.map.height
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

  def drawSelectionOutline(canvas: Canvas): Unit = {
    val context = canvas.getGraphicsContext2D
    selectedCharacter.foreach(character => {
      val player = game.playerList.indexOf(game.playerList.find(_.characters.exists(_ == character)).get)
      val walkOffset = Coordinate.fromDirection(character.direction) * character.walkingOffset * 2
      context.setStroke(GameScreen.PlayerColors(player))
      context.strokeRect(character.position.x * Tile.Size - walkOffset.x, character.position.y * Tile.Size - walkOffset.y, Tile.Size, Tile.Size)
    })
  }

  def drawGameCharacters(canvas: Canvas): Unit = {
    val context = canvas.graphicsContext2D
    context.save()
    game.playerList.flatMap(_.characters).foreach(character => {
      val walkOffset = Coordinate.fromDirection(character.direction) * character.walkingOffset * 2
      val xPos = character.position.x * Tile.Size - walkOffset.x
      val yPos = character.position.y * Tile.Size - walkOffset.y
      context.drawImage(characterImageMap(character.charType.toString),
          character.frame * Tile.Size, character.direction.id * GameScreen.CharacterHeight, Tile.Size, GameScreen.CharacterHeight,
          xPos, yPos, Tile.Size, Tile.Size
          )
      //draw a player colored badge in the upper-right corner
      GameScreen.PlayerColors.lift(game.characterPlayer(character)).foreach(color => {
        context.fill = color
        context.stroke = White
        context.lineWidth = 1
        context.strokeRect(xPos + GameScreen.PlayerBadgeOffset, yPos + 1, GameScreen.PlayerBadgeSize, GameScreen.PlayerBadgeSize)
        context.fillRect(xPos + GameScreen.PlayerBadgeOffset, yPos + 1, GameScreen.PlayerBadgeSize, GameScreen.PlayerBadgeSize)
      })
    })
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
    val context = canvas.getGraphicsContext2D
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

  def deselectDeadCharacter(): Unit = {
    selectedCharacter = if (selectedCharacter.map(_.isDead).getOrElse(false)) None else selectedCharacter
  }
}

object GameScreen {

  val PlayerColors = Array(Blue, Red, Green, Yellow)

  val CharacterHeight = 36
  val TileMapWidth = 8
  val ScrollSpeed = 16
  val TickDelay = 17 //milliseconds
  val MenuWidth = 200
  val PlayerBadgeOffset = 27
  val PlayerBadgeSize = 4

}
