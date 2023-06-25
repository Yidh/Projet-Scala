package particule

import javafx.scene.input.KeyCode
import common.Constant.*
import common.Direction
import particule.Main.stage
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.Circle
import scalafx.stage.Screen
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

object Main extends JFXApp3 {

  val PARTICULES_RADIUS = 5
  val NUMBER_PARTICULES = 100


  def generatePaticules(n: Int, width: Int, height: Int): List[Particule] =
    List.fill(n) {
      val x = Random.nextInt(width)
      val y = Random.nextInt(height)
      val numberDirections = Direction.values.length
      val direction = Direction.values(Random.nextInt(numberDirections))
      val color = Color.rgb(Random.nextInt(256), Random.nextInt(256), Random.nextInt(256), 1)
      Particule(x, y, direction, color)
    }

  def gameloop(update: () => Unit): Unit = {
    def tick = Future {
      update();
      Thread.sleep(100)
    }

    def loopagain = Future(gameloop(update))
    for {
      _ <- tick
      _ <- loopagain
    } yield ()
  }


  override def start(): Unit = {

    val state = ObjectProperty(State(generatePaticules(NUMBER_PARTICULES, BOARD_WIDTH , BOARD_HEIGHT)))
    val frame = IntegerProperty(0)

    frame.onChange {
      val allParticles = state.value.particules.groupBy(p => (p.x, p.y))
      state.update(state.value.newState(allParticles))
    }

    stage = new PrimaryStage {
      title = "Particules"
      width = BOARD_WIDTH
      height = BOARD_HEIGHT
      scene = new Scene {
        fill = Black
        content = state.value.drawParticules()
        frame.onChange(Platform.runLater {
          content = state.value.drawParticules()

        })

      }
      gameloop(() => frame.update(frame.value + 1))
    }
  }
}