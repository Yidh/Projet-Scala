package wator

import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import wator.{Shark, State, Thon}
import WatorAgentType.Thon
import WatorAgentType.Shark

case class State(thon: List[Thon], shark: List[Shark]) {
  type Coordinates = (Int,Int)
  val THON_RADIUS = 2
  val SHARK_RADIUS = 4

  def newThonState(allThon: Map[Coordinates, Thon], allShark: Map[Coordinates, List[Shark]]): List[Thon] = {
    val Fishes  = allThon.map { case (coordinates, _) => coordinates -> Thon }
    thon.flatMap(_.move(allThon, Fishes))
  }

 def newSharkState(allShark: Map[Coordinates,Shark],allthon: Map[Coordinates, List[Thon]]) :List[Shark]= {
   val Fishes = allShark.map{case (coordinates, _) => coordinates -> Shark}   
   shark.flatMap(_.move(allShark,Fishes))
 }

  def drawThon(): List[Circle] =
    thon.map { t =>
      new Circle {
        centerX = t.x
        centerY = t.y
        radius = THON_RADIUS
        fill = t.color
      }
    }

  def drawShark(): List[Circle] =
    shark.map { s =>
      new Circle {
        centerX = s.x
        centerY = s.y
        radius = SHARK_RADIUS
        fill = s.color
      }
    }
}
