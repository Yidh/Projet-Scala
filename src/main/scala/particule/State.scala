package particule

import common.Constant.*
import particule.Main.PARTICULES_RADIUS
import particule.Position.{Coordinates, nextPosition}
import scalafx.scene.shape.Circle
case class State (particules: List[Particule]) {
  
  def newState(allParticles: Map[Coordinates, List[Particule]]): State =
    copy(particules = particules.map(_.move(allParticles)))
 
  def drawParticules(): List[Circle] =
    particules.map { p =>
      new Circle {
        centerX = p.x
        centerY = p.y
        radius = PARTICULES_RADIUS
        fill = p.color
      }
    }
}
