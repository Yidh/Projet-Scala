package particule

import common.Direction
import common.Direction.*
object Position {

  type Coordinates = (Int, Int)
  def nextPosition(x:Int, y:Int, direction: common.Direction, boardWidth: Int, boardHeight: Int): Coordinates = {
    val (xMin, xMax, yMin, yMax) = (0, boardWidth - 1, 0, boardHeight - 1)
    (direction, x, y) match {
      case (NORTH, _, `yMin`) => (x, yMax) // tout en haut => tout en bas
      case (SOUTH, _, `yMax`) => (x, yMin) // tout en bas => tout en haut
      case (EAST, `xMax`, _) => (xMin, y) // tout à droite => tout à gauche
      case (WEST, `xMin`, _) => (xMax, y) // tout à gauche => tout à droite
      case (NORTH_EAST, `xMax`, `yMin`) => (xMin, yMax) // tout en haut à droite => tout en bas à gauche
      case (NORTH_EAST, `xMax`, _) => (xMin, y)
      case (NORTH_EAST, _, `yMin`) => (x, yMax)
      case (NORTH_WEST, `xMin`, `yMin`) => (xMax, yMax) // tout en haut à gauche => tout en bas à droite
      case (NORTH_WEST, `xMin`, _) => (xMax, y)
      case (NORTH_WEST, _, `yMin`) => (x, yMax)
      case (SOUTH_EAST, `xMax`, `yMax`) => (xMin, yMin) // tout en bas à droite => tout en haut à gauche
      case (SOUTH_EAST, `xMax`, _) => (xMin, y)
      case (SOUTH_EAST, _, `yMax`) => (x, yMin)
      case (SOUTH_WEST, `xMin`, `yMax`) => (xMax, yMin) // tout en bas à gauche => tout en haut à droite
      case (SOUTH_WEST, `xMin`, _) => (xMax, y)
      case (SOUTH_WEST, _, `yMax`) => (x, yMin)
      case (NORTH, _, _) => (x, y - 1)
      case (SOUTH, _, _) => (x, y + 1)
      case (EAST, _, _) => (x + 1, y)
      case (WEST, _, _) => (x - 1, y)
      case (NORTH_EAST, _, _) => (x + 1, y - 1)
      case (NORTH_WEST, _, _) => (x - 1, y - 1)
      case (SOUTH_EAST, _, _) => (x + 1, y + 1)
      case (SOUTH_WEST, _, _) => (x - 1, y + 1)
    }
  }

  def neighboursOf(x: Int, y: Int, distance: Int): Seq[Coordinates] =
    for {
      i <- -distance to distance // renvoie une liste (range)
      j <- -distance to distance if (i, j) != (0, 0)
    } yield (x + i, y + j)

}
