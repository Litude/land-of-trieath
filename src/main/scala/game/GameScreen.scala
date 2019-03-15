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

class GameScreen extends BaseScreen {
  
  val PlayerColors = Array(Blue, Red, Green, Yellow)
  
  val TileSize = 32
  val CharacterHeight = 36
  val TileMapWidth = 8
  val ScrollSpeed = 16
  val TickDelay = 17 //milliseconds
  val MenuWidth = 200
  
  var selectedCharacter: Option[Character] = None
  
  var camOffset = Coordinate(0, 0)
  var centerPosition = Coordinate(0, 0)
  var mousePosition = Coordinate(0, 0)
  var damageCounters = Array[DamageCounter]()
  
  fill = Black
      
  val game = new Game(onDamageCaused)
  
  val characterImageMap = new HashMap[String, Image]()
  characterImageMap += (Warrior.toString -> new Image("file:img/warrior_m.png"))
  characterImageMap += (Monk.toString -> new Image("file:img/townfolk1_m.png"))

  val mapCanvas = new Canvas(game.map.width * TileSize, game.map.height * TileSize)
  val characterCanvas = new Canvas(game.map.width * TileSize, game.map.height * TileSize)
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
  val testChar = new Character(100, Warrior)
  val testPlayer = new Player("tester")
  testPlayer.characters += testChar
  val testChar2 = new Character(100, Monk)
  testChar2.moveTo(Coordinate(6, 3))
  testPlayer.characters += testChar2
  game.playerList += testPlayer
  
  val testPlayer2 = new Player("other")
  val otherChar1 = new Character(30, Monk)
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
        drawSelectionOutline(characterCanvas)
        drawGameCharacters(characterCanvas)
        drawDamageCounters(characterCanvas, TickDelay)
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
        }
        case MouseButton.Secondary => {
          selectedCharacter.foreach(character => {
            if (game.playerList(game.currentPlayer).characters.exists(_ == character)) {
              game.moveCharacter(character, tileCoords)
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
  
  menu.onMouseClicked = mouseEvent => {
    println("clicked menu")
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

  def drawGameMap(canvas: Canvas): Unit = {

    def drawTileOnCanvas(context: GraphicsContext, image: Image, index: Int, x: Int, y: Int): Unit = {
      context.drawImage(image,
          (index % TileMapWidth) * TileSize, (index / TileMapWidth) * TileSize, TileSize, TileSize,
          x * TileSize, y * TileSize, TileSize, TileSize
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
      context.strokeRect(character.position.x * TileSize - walkOffset.x, character.position.y * TileSize - walkOffset.y, TileSize, TileSize)
    })
  }
  
  def drawGameCharacters(canvas: Canvas): Unit = {
    val context = canvas.graphicsContext2D
    game.playerList.flatMap(_.characters).foreach(character => {
      val walkOffset = Coordinate.fromDirection(character.direction) * character.walkingOffset * 2
      context.drawImage(characterImageMap(character.charType.toString),
          character.frame * TileSize, character.direction.id * CharacterHeight, TileSize, CharacterHeight,
          character.position.x * TileSize - walkOffset.x, character.position.y * TileSize - walkOffset.y, TileSize, TileSize
          )
    })
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
      if (width.get.toInt - MenuWidth > game.map.width * TileSize) {
        camOffset = Coordinate(-MenuWidth / 2, camOffset.y)
      }
      if (height.get.toInt > game.map.height * TileSize) {
        camOffset = Coordinate(camOffset.x, 0)
      }
  }
  
  def onResize(): Unit = {
    centerPosition = Coordinate(width.get.toInt / 2 - game.map.width / 2 * TileSize, height.get.toInt / 2 - game.map.height / 2 * TileSize)
    clipCameraToBounds()
    translateScene()
    mapPane.setMinWidth(width.get - MenuWidth)
    mapPane.setMaxWidth(width.get - MenuWidth)
  }
  
  def worldToTile(world: Point2D): Coordinate = worldToTile(Coordinate(world.x.toInt, world.y.toInt))
  
  def worldToTile(world: Coordinate) = Coordinate(world.x / TileSize, world.y / TileSize)
  
  def translateScene(): Unit = {
    Array(mapCanvas, characterCanvas).foreach(canvas => {
      canvas.translateX = camOffset.x + centerPosition.x
      canvas.translateY = camOffset.y + centerPosition.y
    })
  }

  def onDamageCaused(damage: Int, position: Coordinate) = {
    this.damageCounters = this.damageCounters :+ new DamageCounter(damage, position)
  }
  def drawDamageCounters(canvas: Canvas, delay: Int) = {
    this.damageCounters.foreach(counter => {
      counter.offset += delay / 20.0
      counter.ttl -= delay
    })
    this.damageCounters = this.damageCounters.filter(_.ttl > 0)

    val context = canvas.graphicsContext2D
    context.save()
    context.fill = Red
    context.stroke = Black
    context.lineWidth = 2

    this.damageCounters.foreach(counter => {
      val string = counter.damage.toString
      val bounds = getContextStringBounds(context, string)
      val textWidth = bounds.getWidth
      val textHeight = bounds.getHeight

      context.strokeText(string,
          counter.position.x * TileSize + (TileSize - textWidth) / 2, counter.position.y * TileSize + (TileSize + textHeight) / 2 - counter.offset, TileSize
          )
      context.fillText(string,
          counter.position.x * TileSize + (TileSize - textWidth) / 2, counter.position.y * TileSize + (TileSize + textHeight) / 2 - counter.offset, TileSize
          )
    })
    context.restore()
  }

  def getContextStringBounds(context: GraphicsContext, text: String) = {
    val metricText = new Text
    metricText.text = text
    metricText.font = context.font
    metricText.strokeWidth = context.lineWidth
    metricText.layoutBounds.get
  }
  
  def deselectDeadCharacter() = {
    selectedCharacter = if (selectedCharacter.map(_.isDead).getOrElse(false)) None else selectedCharacter
  }
}

class DamageCounter(val damage: Int, val position: Coordinate) {
  var offset = 0.0
  var ttl = 1000
}
