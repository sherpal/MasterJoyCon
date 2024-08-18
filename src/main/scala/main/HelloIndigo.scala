package main

import indigo.*
import custommath.Complex

import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js

//noinspection ScalaUnusedSymbol
@JSExportTopLevel("IndigoGame") // Pandering to mdoc
object HelloIndigo extends IndigoSandbox[Unit, Model] {

  val config: GameConfig =
    GameConfig.default

  private val assetName = AssetName("dots")
  private val fontName  = AssetName("quicksand")

  val assets: Set[AssetType] =
    Set(
      AssetType.Image(assetName, AssetPath("assets/dots.png")),
      AssetType.Font(fontName, AssetPath("assets/Quicksand-VariableFont_wght.ttf"))
    )

  def initialModel(startupData: Unit): Outcome[Model] =
    Outcome(
      Model.initial(
        config.viewport.bounds
      )
    )

  def updateModel(
      context: FrameContext[Unit],
      model: Model
  ): GlobalEvent => Outcome[Model] = model.updateModel(context)

  def present(
      context: FrameContext[Unit],
      model: Model
  ): Outcome[SceneUpdateFragment] = {
    val gameAreaRectangle =
      Shape.Box(model.bounds, fill = Fill.None, stroke = Stroke(2, RGBA.White))
    model match {
      case model: EndOfGameModel =>
        val gameOverText =
          TextBox(s"Game Over! Tu as tenu ${model.endOfGameTime.toInt} secondes!", 500, 30)
            .moveTo(config.viewport.center)
            .withFontFamily(FontFamily("quicksand"))
            .withFontSize(Pixels(20))
            .withColor(RGBA.Magenta)
        Outcome(
          SceneUpdateFragment(
            gameAreaRectangle,
            gameOverText
          )
        )
      case model: GameRunningModel =>
        val bulletGraphics = model.bullets.map { bullet =>
          val pos = bullet.currentPosition(model.gameTime)
          Shape.Circle(
            center = model.changeCoordinates(pos),
            radius = bullet.radius.toInt,
            fill = Fill.Color(RGBA.Red),
            stroke = Stroke.apply(2, RGBA.White)
          )
        }
        val playerGraphic = Shape.Circle(
          center = model.changeCoordinates(model.player.currentPosition),
          radius = model.player.radius.toInt,
          fill = Fill.Color(RGBA.Magenta),
          stroke = Stroke.apply(2, RGBA.White)
        )
        Outcome(
          SceneUpdateFragment(
            gameAreaRectangle +: playerGraphic +: bulletGraphics*
          )
        )
    }
  }

  override def fonts: Set[FontInfo] = Set.empty

  override def animations: Set[Animation] = Set.empty

  override def shaders: Set[Shader] = Set.empty

  override def setup(assetCollection: AssetCollection, dice: Dice): Outcome[Startup[Unit]] =
    Outcome(Startup.Success(()))
}

sealed trait Model {
  def updateModel(context: FrameContext[Unit]): GlobalEvent => Outcome[Model]

  def bounds: Rectangle
}

//noinspection ScalaWeakerAccess
final case class GameRunningModel(
    bounds: Rectangle,
    player: Player,
    bullets: List[Bullet],
    lastAddedBullet: Seconds,
    gameTime: Seconds
) extends Model {

  val level = 5

  val spawnBulletEvery: Seconds = Seconds(3.0 / level)

  /*
  Change of coordinates between complex and point

  Scales are 1:1, but the origin for complex corresponds to the center of the bounds in the point system, and
  complex grow to the right and to the top, while points grow to the right and the bottom
   */

  def changeCoordinates(z: Complex): Point = {
    val x = z.re + bounds.center.x
    val y = bounds.center.y - z.im
    Point(x.toInt, y.toInt)
  }

  def changeCoordinates(p: Point): Complex = {
    val x = p.x - bounds.center.x
    val y = bounds.center.y - p.y
    Complex(x, y)

  }

  val topRight: Complex    = changeCoordinates(bounds.topRight)
  val bottomLeft: Complex  = changeCoordinates(bounds.bottomLeft)
  val topLeft: Complex     = changeCoordinates(bounds.topLeft)
  val bottomRight: Complex = changeCoordinates(bounds.bottomRight)
  val center: Complex      = 0

  private val radius2 = (topRight - center).modulus2

  private val corners = Vector(bottomLeft, bottomRight, topRight, topLeft, bottomLeft)

  private def randomBullet(dice: Dice): Bullet = {
    val side             = dice.roll(4)
    val refCorner        = corners(side - 1)
    val nextCorner       = corners(side)
    val dir              = nextCorner - refCorner
    val startingPosition = refCorner + dice.roll(1000) / 1000.0 * dir

    val dirAngle  = dir.arg
    val direction = dirAngle + dice.roll(1000) / 1000.0 * Math.PI

    Bullet(startingPosition, Complex.rotation(direction), 100, gameTime)
  }

  private def cropPositionToBounds(pos: Complex): Complex = {
    val re =
      if pos.re < topLeft.re then topLeft.re
      else if pos.re > bottomRight.re then bottomRight.re
      else pos.re
    val im =
      if pos.im > topLeft.im then topLeft.im
      else if pos.im < bottomRight.im then bottomRight.im
      else pos.im

    Complex(re, im)
  }

  def withRandomBullet(dice: Dice): GameRunningModel =
    copy(bullets = randomBullet(dice) +: bullets, lastAddedBullet = gameTime)

  def withTooFarBulletsRemoved: GameRunningModel = {
    val newBullets =
      bullets
        .filter(b => (b.currentPosition(gameTime) - center).modulus2 < 1.5 * radius2)
    this.copy(bullets = newBullets)
  }

  def update(
      timeDelta: Seconds,
      maybePlayerDirection: Option[Double]
  ): GameRunningModel =
    this.copy(
      player = player.update(timeDelta, maybePlayerDirection, cropPositionToBounds),
      gameTime = gameTime + timeDelta
    )

  override def updateModel(context: FrameContext[Unit]): GlobalEvent => Outcome[Model] = {
    case FrameTick =>
      inline def keyIsDown(key: Key): Boolean = context.keyboard.keysDown.contains[Key](key)

      val playerX: Int =
        (if keyIsDown(Key.RIGHT_ARROW) then 1 else 0) +
          (if keyIsDown(Key.LEFT_ARROW) then -1 else 0)

      val playerY: Int =
        (if keyIsDown(Key.UP_ARROW) then 1 else 0) +
          (if keyIsDown(Key.DOWN_ARROW) then -1 else 0)

      val maybePlayerDirection =
        if playerX == 0 && playerY == 0 then None else Some(Math.atan2(playerY, playerX))

      val modelWithNewBullet =
        if gameTime - lastAddedBullet > spawnBulletEvery then withRandomBullet(context.dice)
        else this

      val modelWithNewBulletAndUpdatedPlayer =
        modelWithNewBullet.update(context.delta, maybePlayerDirection).withTooFarBulletsRemoved

      if modelWithNewBulletAndUpdatedPlayer.bullets.exists(_.hitPlayer(gameTime, player)) then
        Outcome(EndOfGameModel(bounds, gameTime))
      else Outcome(modelWithNewBulletAndUpdatedPlayer)
    case _ =>
      Outcome(this)
  }
}

final case class EndOfGameModel(bounds: Rectangle, endOfGameTime: Seconds) extends Model {

  override def updateModel(context: FrameContext[Unit]): GlobalEvent => Outcome[Model] = {
    case event: KeyboardEvent if event.keyCode == Key.ENTER =>
      Outcome(Model.initial(bounds))
    case _ => Outcome(this)
  }
}

object Model {
  def initial(bounds: Rectangle): Model =
    GameRunningModel(bounds, Player(Complex.zero, 150), List.empty, Seconds(0), Seconds(0))

}

case class Bullet(
    startingPosition: Complex,
    direction: Complex,
    speed: Double,
    spawnTime: Seconds
) {
  def currentPosition(currentTime: Seconds): Complex = {
    val timeDelta = currentTime - spawnTime
    startingPosition + direction * speed * timeDelta.toDouble
  }

  val radius = 8.0

  def hitPlayer(currentTime: Seconds, player: Player): Boolean = {
    val currentPosition = this.currentPosition(currentTime)
    val playerPosition  = player.currentPosition
    (currentPosition - playerPosition).modulus2 < (radius + player.radius) * (radius + player.radius)
  }
}

case class Player(currentPosition: Complex, speed: Double) {
  val radius = 8.0

  def update(
      timeDelta: Seconds,
      maybeMovingDirection: Option[Double],
      cropPosition: Complex => Complex
  ): Player = {
    maybeMovingDirection match {
      case None => this
      case Some(direction) =>
        this.copy(
          currentPosition =
            cropPosition(currentPosition + Complex.rotation(direction) * speed * timeDelta.toDouble)
        )
    }
  }
}
