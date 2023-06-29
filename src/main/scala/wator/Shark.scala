package wator

import scalafx.scene.paint.Color
import wator.WatorAgentType

import scala.util.Random

case class Shark(x: Int, y: Int, color: Color, sbreed: Int, sbreedcounter: Int, senergy: Int) {
  type Coordinates = (Int, Int)
  val fishesRadius: Int = 5
  val fishesDiameter: Int = fishesRadius * 2
  val boardSize: Int = 100

  def move(allShark: Map[Coordinates, Shark], Fishes: Map[Coordinates, WatorAgentType]): List[Shark] = {
    val neighbours: Seq[Coordinates] = Random.shuffle(neighboursOf(x, y, fishesDiameter))

    val tunaNeighbours: Option[Coordinates] = neighbours.find { case (nx, ny) => Fishes.get((nx, ny)).contains(WatorAgentType.Thon) }
    val freeNeighbours: Option[Coordinates] = neighbours.find { case (nx, ny) => !Fishes.contains((nx, ny)) }

    (tunaNeighbours, freeNeighbours) match {
      case (Some((tunaX, tunaY)), _) =>
        // Déplacer vers une case voisine occupée par un thon
        val newEnergy = senergy + 1 // Le requin gagne un point d'énergie en mangeant un thon
        val newBreedCounter = if (newEnergy >= sbreed) 0 else sbreedcounter + 1 // Vérifier si le requin peut se reproduire
        val updatedShark = copy(x = tunaX, y = tunaY, sbreedcounter = newBreedCounter, senergy = newEnergy)

        if (newBreedCounter >= sbreed) {
          // Se reproduire en donnant naissance à un nouveau requin dans la case d'origine
          val offspring = Shark(x, y, color, sbreed, sbreedcounter = 0, senergy)
          List(updatedShark, offspring)
        } else {
          List(updatedShark)
        }

      case (_, Some((freeX, freeY))) =>
        // Déplacer vers une case voisine libre
        val newEnergy = senergy - 1 // Le requin perd un point d'énergie à chaque cycle s'il ne mange pas de thon
        val newBreedCounter = if (newEnergy >= sbreed) 0 else sbreedcounter + 1 // Vérifier si le requin peut se reproduire
        val updatedShark = copy(x = freeX, y = freeY, sbreedcounter = newBreedCounter, senergy = newEnergy)

        if (newBreedCounter >= sbreed) {
          // Se reproduire en donnant naissance à un nouveau requin dans la case d'origine
          val offspring = Shark(x, y, color, sbreed, sbreedcounter = 0, senergy)
          List(updatedShark, offspring)
        } else {
          List(updatedShark)
        }

      case _ =>
        // Ne bouge pas, ne se reproduit pas et perd un point d'énergie
        val newEnergy = senergy - 1
        if (newEnergy <= 0) {
          // Le requin n'a plus d'énergie et meurt
          List.empty
        } else {
          val updatedShark = copy(senergy = newEnergy)
          List(updatedShark)
        }
    }
  }

  private def neighboursOf(x: Int, y: Int, distance: Int): Seq[Coordinates] =
    for {
      i <- -distance to distance
      j <- -distance to distance if (i, j) != (0, 0) &&
        (0 until boardSize).contains(x + i) &&
        (0 until boardSize).contains(y + j)
    } yield (x + i, y + j)

  def decreaseEnergy(): Shark = {
    copy(senergy = senergy - 1)
  }
}
