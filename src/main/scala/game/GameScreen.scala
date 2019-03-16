package game

import scalafx.Includes._
import scalafx.animation._
import scalafx.scene.paint.Color._
import scalafx.scene.image.Image
import scalafx.scene.canvas._
import scalafx.util.Duration
import scalafx.scene.layout._
import scalafx.scene.input._
import scalafx.geometry.Point2D
import scalafx.scene.text.Text

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class GameScreen extends BaseScreen {
  
  val PlayerColors = Array(Blue, Red, Green, Yellow)
  
  val CharacterHeight = 36
  val TileMapWidth = 8
  val ScrollSpeed = 16
  val TickDelay = 17 //milliseconds
  val MenuWidth = 200
  
  var selectedCharacter: Option[Character] = None
  var hoveredTile = Coordinate(0 ,0)
  
  var camOffset = Coordinate(0, 0)
  var centerPosition = Coordinate(0, 0)
  var mousePosition = Coordinate(0, 0)
  val tileEffects = new TileEffects()
  var reachableTiles = ArrayBuffer[Coordinate]()
  
  fill = Black
      
  val game = new Game(onDamageCaused)
  
  val characterImageMap = new HashMap[String, Image]()
  characterImageMap += (Warrior.toString -> new Image("file:img/warrior_m.png"))
  characterImageMap += (Monk.toString -> new Image("file:img/townfolk1_m.png"))

  val mapCanvas = new Canvas(game.map.width * Tile.Size, game.map.height * Tile.Size)
  val characterCanvas = new Canvas(game.map.width * Tile.Size, game.map.height * Tile.Size)
  drawGameMap(mapCanvas)
  
  val mapPane = new Pane
  mapPane.children = Array(mapCanvas, characterCanvas)
  mapPane.hgrow = Priority.Always
  
  val menu = new GameSideBar(MenuWidth, endTurn)
  menu.updateCurrentPlayerText(game.currentPlayer)
  
  val layout = new HBox
  layout.children = Array(mapPane, menu)
  content = layout
 
  //add some test data
  val testChar = new Character(100, Warrior, Direction.South)
  val testPlayer = new Player("tester", Human)
  testPlayer.characters += testChar
  val testChar2 = new Character(100, Monk, Direction.South)
  testChar2.moveTo(Coordinate(6, 3))
  testPlayer.characters += testChar2
  game.playerList += testPlayer
  
  val testPlayer2 = new Player("other", Computer)
  val otherChar1 = new Character(30, Monk, Direction.North)
  otherChar1.moveTo(Coordinate(3, 6))
  testPlayer2.characters += otherChar1
  game.playerList += testPlayer2
  
  val gameLoop = new Timeline {
    keyFrames = Seq(
      KeyFrame(Duration(TickDelay), onFinished = () => {
        game.updateGameState()
        deselectDeadCharacter()
        menu.updateCharacterUI(selectedCharacter, selectedCharacter.map(game.getCharacterPlayer))
        clearCanvas(characterCanvas)
        highlightHoveredTile(characterCanvas)
        updateReachableTiles(characterCanvas)
        drawSelectionOutline(characterCanvas)
        drawGameCharacters(characterCanvas)
        drawScreenEffects(characterCanvas, TickDelay)
      })
    )
    cycleCount = Timeline.Indefinite
  }
  gameLoop.play()
  
  mapPane.onMouseClicked = mouseEvent => {
    //check that we clicked on the map canvas
    if (mapCanvas.contains(mapCanvas.parentToLocal(mouseEvent.sceneX, mouseEvent.sceneY))) {
      val worldCoords = mapCanvas.parentToLocal(mouseEvent.sceneX, mouseEvent.sceneY)
      val tileCoords = worldToTile(worldCoords)
      
      mouseEvent.button match {
        case MouseButton.Primary => {
          selectedCharacter = game.playerList.flatMap(_.characters).find(_.position == tileCoords)
          reachableTiles = selectedCharacter match {
            case Some(character) if (game.getCharacterPlayer(character) == game.currentPlayer) => game.getReachableCharacterTiles(character)
            case _ => ArrayBuffer()
          }
        }
        case MouseButton.Secondary => {
          selectedCharacter.foreach(character => {
            if (game.playerList(game.currentPlayer).characters.exists(_ == character)) {
              if (game.moveCharacter(character, tileCoords) == MovementResult.Attacking) {
                this.tileEffects += new HighlightEffect(tileCoords)
                
              }
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
        case KeyCode.Left => camOffset = Coordinate(camOffset.x + ScrollSpeed, camOffset.y)
        case KeyCode.Right => camOffset = Coordinate(camOffset.x - ScrollSpeed, camOffset.y)
        case KeyCode.Up => camOffset = Coordinate(camOffset.x, camOffset.y + ScrollSpeed)
        case KeyCode.Down => camOffset = Coordinate(camOffset.x, camOffset.y - ScrollSpeed)
        case _ =>
      }
    clipCameraToBounds()
    translateScene()
  }
  
  mapPane.onMouseMoved = mouseEvent => {
    hoveredTile = worldToTile(Coordinate(mouseEvent.sceneX.toInt, mouseEvent.sceneY.toInt))
  }

  def drawGameMap(canvas: Canvas): Unit = {

    def drawTileOnCanvas(context: GraphicsContext, image: Image, index: Int, x: Int, y: Int): Unit = {
      context.drawImage(image,
          (index % TileMapWidth) * Tile.Size, (index / TileMapWidth) * Tile.Size, Tile.Size, Tile.Size,
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
      menu.updateCurrentPlayerText(game.currentPlayer)
    }
  }
  
  def drawSelectionOutline(canvas: Canvas): Unit = {
    val context = canvas.getGraphicsContext2D
    selectedCharacter.foreach(character => {
      val player = game.playerList.indexOf(game.playerList.find(_.characters.exists(_ == character)).get)
      val walkOffset = Coordinate.fromDirection(character.direction) * character.walkingOffset * 2
      context.setStroke(PlayerColors(player))
      context.strokeRect(character.position.x * Tile.Size - walkOffset.x, character.position.y * Tile.Size - walkOffset.y, Tile.Size, Tile.Size)
    })
  }
  
  def drawGameCharacters(canvas: Canvas): Unit = {
    val context = canvas.graphicsContext2D
    game.playerList.flatMap(_.characters).foreach(character => {
      val walkOffset = Coordinate.fromDirection(character.direction) * character.walkingOffset * 2
      context.drawImage(characterImageMap(character.charType.toString),
          character.frame * Tile.Size, character.direction.id * CharacterHeight, Tile.Size, CharacterHeight,
          character.position.x * Tile.Size - walkOffset.x, character.position.y * Tile.Size - walkOffset.y, Tile.Size, Tile.Size
          )
    })
  }
  
  def highlightHoveredTile(canvas: Canvas): Unit = {
    highlightTiles(canvas, Seq(hoveredTile))
  }
  
  def updateReachableTiles(canvas: Canvas): Unit = {
    selectedCharacter match {
      case Some(character) if (character.isMoving) => {
        reachableTiles.clear()
      }
      case Some(character) if (reachableTiles.isEmpty && game.getCharacterPlayer(character) == game.currentPlayer) => {
        reachableTiles = game.getReachableCharacterTiles(character)
      }
      case None if (!reachableTiles.isEmpty) => reachableTiles.clear()
      case _ =>
    }
    highlightTiles(canvas, reachableTiles)
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
          Math.max(Math.min(camOffset.x, -centerPosition.x), centerPosition.x - MenuWidth),
          Math.max(Math.min(camOffset.y, -centerPosition.y), centerPosition.y)
          )
      if (width.get.toInt - MenuWidth > game.map.width * Tile.Size) {
        camOffset = Coordinate(-MenuWidth / 2, camOffset.y)
      }
      if (height.get.toInt > game.map.height * Tile.Size) {
        camOffset = Coordinate(camOffset.x, 0)
      }
  }
  
  def onResize(): Unit = {
    centerPosition = Coordinate(width.get.toInt / 2 - game.map.width / 2 * Tile.Size, height.get.toInt / 2 - game.map.height / 2 * Tile.Size)
    clipCameraToBounds()
    translateScene()
    mapPane.setMinWidth(width.get - MenuWidth)
    mapPane.setMaxWidth(width.get - MenuWidth)
  }
  
  def worldToTile(world: Point2D): Coordinate = worldToTile(Coordinate(world.x.toInt, world.y.toInt))
  
  def worldToTile(world: Coordinate) = Coordinate(world.x / Tile.Size, world.y / Tile.Size)
  
  def translateScene(): Unit = {
    Array(mapCanvas, characterCanvas).foreach(canvas => {
      canvas.translateX = camOffset.x + centerPosition.x
      canvas.translateY = camOffset.y + centerPosition.y
    })
  }

  def onDamageCaused(damage: Int, position: Coordinate) = {
    this.tileEffects += new DamageCounter(damage, position)
  }
  
  def drawScreenEffects(canvas: Canvas, delay: Int) = {
    this.tileEffects.update(delay)
    this.tileEffects.draw(canvas)
  }
  
  def deselectDeadCharacter() = {
    selectedCharacter = if (selectedCharacter.map(_.isDead).getOrElse(false)) None else selectedCharacter
  }
}
