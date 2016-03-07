package controller

import javafx.fxml.FXML
import javafx.scene.layout.Pane

import control.ConsoleFx
import model.{World}
import org.mtrupkin.core.{Point, Points}

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.input.KeyCode._
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

      consolePane.getChildren.clear()
      consolePane.getChildren.add(console)

      consolePane.setFocusTraversable(true)
    }

    override def update(elapsed: Int): Unit = {
      console.draw(world)
      if (world.player.hitPoints <= 0) {
        changeState(new GameController(World()))
      }
    }

    def handleKeyPressed(event: sfxi.KeyEvent): Unit = {
      event.consume()
      val code = event.code
      val direction = getDirection(code)
      code match {
        case ESCAPE => exit()
        case _ => direction.map(p => world.movePlayer(p))
      }
    }

    def getDirection(code: KeyCode): Option[Point] = {
      import KeyCode._
      code match {
        case W | UP | NUMPAD8 => Option(Points.Up)
        case S | DOWN | NUMPAD2 => Option(Points.Down)
        case A | LEFT | NUMPAD4 => Option(Points.Left)
        case D | RIGHT | NUMPAD6 => Option(Points.Right)
        case NUMPAD1 | END => Option(Points.Down + Points.Left)
        case NUMPAD3 | PAGE_DOWN => Option(Points.Down + Points.Right)
        case NUMPAD7 | HOME => Option(Points.Up + Points.Left)
        case NUMPAD9 | PAGE_UP => Option(Points.Up + Points.Right)
        case _ => None
      }
    }
  }
}


