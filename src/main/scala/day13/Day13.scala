package day13

import java.util.concurrent.LinkedBlockingQueue

import common.IntCodeComputer._
import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Label
import scalafx.scene.image.Image
import scalafx.scene.input._
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontWeight}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object Day13 extends JFXApp {
  private val scale = 50
  private val (empty, wall, block, paddle, ball) = (0, 1, 2, 3, 4)
  private val canvas = new Canvas(43 * scale, 23 * scale)
  private val scoreLabel = new Label()
  private val gc = canvas.graphicsContext2D
  private val input = new LinkedBlockingQueue[BigInt]()
  private val outputBuffer = mutable.ArrayBuffer[Int]()
  private var lastInput = 0
  private var ballXCoord = 0
  private var paddleXCoord = 0

  private val output = new LinkedBlockingQueue[BigInt]() {
    override def add(e: BigInt): Boolean = {
      outputBuffer.append(e.toInt)
      handleFrame()
      super.add(e)
    }
  }

  stage = new PrimaryStage {
    title = "Day 13 Game"

    scoreLabel.setFont(Font.font("Verdana", FontWeight.Bold, 70))

    val vBox: VBox = new VBox() {
      children = Seq(
        scoreLabel,
        canvas
      )
    }
    vBox.setAlignment(Pos.Center)

    scene = new Scene {
      content = vBox
      onKeyPressed = (ke: KeyEvent) => {
        ke.code match {
          case KeyCode.Left => lastInput = -1
          case KeyCode.Right => lastInput = 1
          case _ => lastInput = 0
        }
      }
    }
  }

  private def handleFrame(): Unit = {
    if (outputBuffer.size == 3) {
      val x = outputBuffer(0)
      val y = outputBuffer(1)
      val r = outputBuffer(2)
      if (x == -1 && y == 0) Platform.runLater {
        scoreLabel.text = s"Score: $r"
        println(r)
      } else Platform.runLater {
        r match {
          case `empty` =>
            gc.fill = Color.White
            gc.fillRect(x * scale, y * scale, scale, scale)
          case `wall` =>
            gc.drawImage(new Image("/brick.png"), x * scale, y * scale, scale, scale)
          case `block` =>
            gc.drawImage(new Image("/diamond.png"), x * scale, y * scale, scale, scale)
          case `paddle` =>
            gc.fill = Color.Aquamarine
            gc.fillRect(x * scale, y * scale, scale, scale)
          case `ball` =>
            gc.fill = Color.DarkCyan
            gc.fillOval(x * scale, y * scale, scale, scale)
        }
      }

      if (r == ball) ballXCoord = x
      else if (r == paddle) paddleXCoord = x
      lastInput = ballXCoord.compare(paddleXCoord)

      Thread.sleep(1000 / 120)

      outputBuffer.clearAndShrink()
    }

    input.clear()
    input.add(lastInput)
  }

  Future {
    val intCode = readIntCode("day13.txt")
    intCode(0) = 2 // insert coin
    input.add(0)
    compute(input, output)(State(intCode))
  }.onComplete {
    case Failure(exception) =>
      exception.printStackTrace()
      Platform.runLater(stage.close())
    case Success(_) =>
      Platform.runLater(stage.close())
  }
}