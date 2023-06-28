package wator

import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import wator.{Shark, State, Thon}
import wator.WatorAgentType


case class State(thon: List[Thon], shark: List[Shark]) {
  type Coordinates = (Int,Int)
 
  val fishesRadius: Int = 5
  val fishesDiameter: Int = fishesRadius * 2

  def newThonState(allThon: Map[Coordinates, Thon], allShark: Map[Coordinates, Shark]): List[Thon] = {
    val Fishes  = allThon.map { case (coordinates, _) => coordinates -> WatorAgentType.Thon }
    thon.flatMap(_.move(allThon, Fishes))
  }

  def newSharkState(allShark: Map[Coordinates, Shark], allThon: Map[Coordinates, Thon]): List[Shark] = {
    val Fishes = allShark.map { case (coordinates, _) => coordinates -> WatorAgentType.Shark }
    allShark.values.flatMap(_.move(allShark, Fishes)).toList
  }



  def drawThon(): List[Circle] =
    thon.map { t =>
      new Circle {
        centerX = t.x * fishesDiameter
        centerY = t.y * fishesDiameter
        radius = fishesRadius
        fill = t.color
      }
    }

  def drawShark(): List[Circle] =
    shark.map { s =>
      new Circle {
        centerX = s.x * fishesDiameter
        centerY = s.y * fishesDiameter
        radius = fishesRadius
        fill = s.color
      }
    }
}
