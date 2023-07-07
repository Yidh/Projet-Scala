package particule

import common.Direction
import common.Direction.*
import common.Constant.*
import particule.Position.*
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.Circle

import scala.util.Random
case class Particule (x:Int, y:Int ,direction: Direction, color: Color) {

  type Coordinates         = (Int,Int)
  val PARTICULES_RADIUS    = 2
  val PARTICULES_DIAMETRE  = PARTICULES_RADIUS *2
  val numberDirections     = Direction.values.length
  val randomDirections     = Direction.values(Random.nextInt(numberDirections))

  def move(allParticles: Map[Coordinates, List[Particule]]): Particule = {
    val (nextX, nextY): Coordinates      = nextPosition(x, y, direction, BOARD_WIDTH, BOARD_HEIGHT)
    val neighbours  : Seq[Coordinates]   = neighboursOf(x, y,PARTICULES_DIAMETRE)
    val collision                        = neighbours.exists(allParticles.contains)
    val newDirection                     = if (collision) randomDirections else direction

    copy(x = nextX, y = nextY, direction = newDirection)
  }


}
