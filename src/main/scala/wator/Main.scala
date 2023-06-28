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

  val NUMBER_THON = 1
  val NUMBER_SHARK = 2
  val THON_COLOR: Color = Brown
  val SHARK_COLOR: Color = Blue
  val T_BREED = 10
  val S_BREED = 10
  val S_ENERGY = 10
  val breedCounter = 1
  val fishesRadius: Int = 5
  val fishesDiameter: Int = fishesRadius * 2
  val boardSize: Int         = 100
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
    val state = ObjectProperty(State(generateThon(NUMBER_THON, boardSize , boardSize ), generateShark(NUMBER_SHARK, boardSize , boardSize )))
    val frame = IntegerProperty(0)

    frame.onChange {
      val allThon = state.value.thon.map(t => (t.x, t.y) -> t).toMap
      val allShark = state.value.shark.map(s => (s.x, s.y)->s).toMap
      state.update(State(state.value.newThonState(allThon, allShark), state.value.newSharkState(allShark, allThon)))
      println("shark" +state.value.shark.size)
      println("thon" +state.value.thon.size)

    }

    stage = new JFXApp3.PrimaryStage {
      title = "WATOR"
      width = boardSize * fishesDiameter
      height = boardSize * fishesDiameter
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
