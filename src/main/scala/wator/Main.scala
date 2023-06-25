import common.Constant.*
import particule.Main.gameloop
import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.Circle
import wator.{Shark, State, Thon}

import scala.util.Random

object Main extends JFXApp3 {

  val NUMBER_THON = 1000
  val NUMBER_SHARK = 1
  val THON_COLOR: Color = Brown
  val SHARK_COLOR: Color = Blue
  val T_BREED = 3
  val S_BREED = 5
  val S_ENERGY = 10000
  val breedCounter = 1
  
  def generateThon(n: Int, width: Int, height: Int): List[Thon] =
    List.fill(n) {
      val x = Random.nextInt(width)
      val y = Random.nextInt(height)
      val color = THON_COLOR
      val tbreed = T_BREED
      val counter = breedCounter
      Thon(x, y, color, tbreed,counter)
    }

  def generateShark(n: Int, width: Int, height: Int): List[Shark] =
    List.fill(n) {
      val x = Random.nextInt(width)
      val y = Random.nextInt(height)
      val color = SHARK_COLOR
      val sbreed = S_BREED
      val senergy = S_ENERGY
      val sbreedcounter = breedCounter
      Shark(x, y, color, sbreed, senergy , sbreedcounter)
    }

  override def start(): Unit = {
    val state = ObjectProperty(State(generateThon(NUMBER_THON, BOARD_WIDTH, BOARD_HEIGHT), generateShark(NUMBER_SHARK, BOARD_WIDTH, BOARD_HEIGHT)))
    val frame = IntegerProperty(0)

    frame.onChange {
      val allThon = state.value.thon.map(t => (t.x, t.y) -> t).toMap
      val allShark = state.value.shark.groupBy(s => (s.x, s.y))
      state.update(State(state.value.newThonState(allThon, allShark), state.value.newSharkState(allThon, allShark)))

    }

    stage = new JFXApp3.PrimaryStage {
      title = "WATOR"
      width = BOARD_WIDTH
      height = BOARD_HEIGHT
      scene = new Scene {
        fill = LightBlue
        content = state.value.drawThon() ++ state.value.drawShark()
        frame.onChange(Platform.runLater {
          content = state.value.drawThon() ++ state.value.drawShark()
        })
      }
      gameloop(() => frame.update(frame.value + 1))
    }
  }
}
