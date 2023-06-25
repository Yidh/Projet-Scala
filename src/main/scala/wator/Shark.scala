import common.Constant
import scalafx.scene.paint.Color
import wator.WatorAgentType

import scala.util.Random

case class Shark(x: Int, y: Int, color: Color, sbreed: Int, sbreedcounter: Int, senergy: Int) {
  type Coordinates = (Int, Int)
  val SHARK_RADIUS = 4
  val SHARK_DIAMETER = SHARK_RADIUS * 2

  def move(Fishes: Map[Coordinates, WatorAgentType], allShark: Map[Coordinates, Shark]): Seq[Shark] = {
    val neighbours: Seq[Coordinates] = neighboursOf(x, y, SHARK_DIAMETER)

    val thonNeighbours = neighbours.filter { case (x, y) => Fishes.contains((x, y)) } // Voisins occupés par un thon
    val freeNeighbours = neighbours.filter { case (x, y) => !Fishes.contains((x, y)) } // Voisins libres

    if (thonNeighbours.nonEmpty) {
      // Déplacer vers une case voisine occupée par un thon
      val (thonX, thonY) = thonNeighbours.head
      List(copy(x = thonX, y = thonY, sbreedcounter = sbreedcounter + 1, senergy = senergy - 1))
    } else if (freeNeighbours.nonEmpty) {
      // Déplacer vers une case voisine libre
      val (freeX, freeY) = freeNeighbours.head

      if (sbreedcounter == sbreed) {
        val child = copy(sbreedcounter = 0)
        val parent = copy(x = freeX, y = freeY, sbreedcounter = 0, senergy = senergy - 1)
        List(parent, child)
      } else {
        List(copy(x = freeX, y = freeY, sbreedcounter = sbreedcounter + 1, senergy = senergy - 1))
      }
    } else {
      List(this)
    }
  }

  private def neighboursOf(x: Int, y: Int, distance: Int): Seq[Coordinates] =
    for {
      i <- -distance to distance
      j <- -distance to distance if (i, j) != (0, 0) &&
        (0 until Constant.BOARD_WIDTH).contains(x + i) &&
        (0 until Constant.BOARD_HEIGHT).contains(y + j)
    } yield (x + i, y + j)

  def decreaseEnergy(): Shark = {
    copy(senergy = senergy - 1)
  }
}
