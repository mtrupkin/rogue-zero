package controller

import javafx.fxml.FXML
import javafx.scene.layout.Pane

import control.ConsoleFx
import model.{World}
import org.mtrupkin.core.{Point, Points}

import scalafx.Includes._
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination}
import scalafx.scene.{control => sfxc, input => sfxi, layout => sfxl, shape => sfxs, text => sfxt}

/**
 * Created by mtrupkin on 12/15/2014.
 */
trait Game { self: Controller =>
  class GameController(world: World) extends ControllerState {
    val name = "Game"

    @FXML var consolePane: Pane = _

    val console = new ConsoleFx()

    def initialize(): Unit = {

      new sfxl.Pane(consolePane) {
        filterEvent(sfxi.KeyEvent.KeyPressed) {
          (event: sfxi.KeyEvent) => handleKeyPressed(event)
        }
      }

      console.setStyle("-fx-border-color: blue")

      consolePane.getChildren.clear()
      consolePane.getChildren.add(console)

      consolePane.setFocusTraversable(true)


    }

    override def update(elapsed: Int): Unit = {
      console.draw(world)
    }

    def handleKeyPressed(event: sfxi.KeyEvent): Unit = {
    }
  }
}


