package controller

import javafx.application.Platform
import javafx.fxml.FXMLLoader
import javafx.scene.control.Label
import javafx.scene.layout.StackPane
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import org.mtrupkin.console.{ConsoleKey, Key, Modifier}
import org.mtrupkin.core.StateMachine

import scala.util.{Failure, Success, Try}
import scalafx.animation.AnimationTimer
import scalafx.scene.{control => sfxc, input => sfxi, layout => sfxl, shape => sfxs, text => sfxt}

/**
 * Created by mtrupkin on 12/15/2014.
 */

trait Controller extends StateMachine
with Game {
  type StateType = ControllerState

  def stage: Stage
  val css: String = "/views/Console.css"

  val viewStack = new StackPane()
  viewStack.getChildren.add(new Label("Loading"))
  val scene = new Scene(initialState.root)

  val cssLocation = getClass.getResource(css).toString
  scene.getStylesheets.add(cssLocation)
  stage.setScene(scene)

  var lastPulse: Long = _
  val timer = AnimationTimer((now: Long) => {
    currentState.update(((now-lastPulse)/100000).toInt)
    lastPulse = now
  })

  timer.start()

  trait ControllerState extends State {
    def name: String
    def templateName: String = s"/views/$name.fxml"

    def root: Parent = {
      val is = getClass.getResourceAsStream(templateName)
      val loader = new FXMLLoader()

      loader.setController(this)
      loader.load[Parent](is)
    }

    def update(elapsed: Int): Unit = {}

    def exit(): Unit = {
      stage.close()
    }

    override def onEnter(): Unit = {
      val t = new Thread(new Runnable {
        override def run(): Unit = {
          val newRoot = root // potentially long-running
          Platform.runLater(new Runnable {
            override def run(): Unit = scene.setRoot(newRoot)
          })

        }
      })

      t.run()
    }
  }
}
