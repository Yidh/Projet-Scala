package wator

import common.Constant
import scalafx.scene.paint.Color

import scala.util.Random

case class Thon(x: Int, y: Int, color: Color, tbreed: Int, breedCounter: Int) {
  type Coordinates = (Int, Int)

  val fishesRadius: Int   = 5
  val fishesDiameter: Int = fishesRadius * 2
  val boardSize: Int      = 100

  def move(allThon: Map[Coordinates, Thon], Fishes: Map[Coordinates, WatorAgentType]) = {
    val neighbours: Seq[Coordinates] = Random.shuffle(neighboursOf(x, y, fishesDiameter))

    val f = neighbours.find { case (x, y) => !Fishes.contains((x, y)) }

    f match {
      case Some((freeX, freeY)) =>
        if (breedCounter == tbreed) {
          val child = copy(breedCounter = 0)
          val parent = copy(x = freeX, y = freeY, breedCounter = 0)
          List(parent, child)
        } else {
          Some(copy(x = freeX, y = freeY, breedCounter = breedCounter + 1))
        }
      case None => List(this)
    }
  }

  def neighboursOf(x: Int, y: Int, distance: Int): Seq[Coordinates] =
    for {
      i <- -distance to distance
      j <- -distance to distance if (i, j) != (0, 0) &&
        (0 until boardSize).contains(x + i) &&
        (0 until boardSize).contains(y + j)
    } yield (x + i, y + j)
}
