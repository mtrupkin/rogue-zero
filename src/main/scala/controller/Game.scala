package controller

import javafx.fxml.FXML
import javafx.scene.control.{TextArea, Label}
import javafx.scene.layout.Pane

import control.ConsoleFx
import model.{WorldBuilder, World}
import org.mtrupkin.core.{Point, Points}

import scalafx.Includes._
import scalafx.scene.input.KeyCode
import scalafx.scene.input.KeyCode._
import scalafx.scene.{control => sfxc, input => sfxi, layout => sfxl, shape => sfxs, text => sfxt}

/**
 * Created by mtrupkin on 12/15/2014.
 */
trait Game { self: Controller =>
  class GameController(world: World) extends ControllerState {
    val name = "Game"

    @FXML var consolePane: Pane = _

    @FXML var hitPoints: Label = _

    @FXML var damage: Label = _

    @FXML var attack: Label = _
    @FXML var modifier: Label = _

    @FXML var defense: Label = _

    @FXML var textArea: TextArea = _


    val console = new ConsoleFx()

    def initialize(): Unit = {
      new sfxl.Pane(consolePane) {
        filterEvent(sfxi.KeyEvent.KeyPressed) {
          (event: sfxi.KeyEvent) => handleKeyPressed(event)
        }
      }

      new sfxl.Pane(console) {
        onMouseClicked = (e: sfxi.MouseEvent) => handleMouseClicked(e)
        onMouseMoved = (e: sfxi.MouseEvent) => handleMouseMove(e)
        onMouseExited = (e: sfxi.MouseEvent) => handleMouseExit(e)
      }

      consolePane.getChildren.clear()
      consolePane.getChildren.add(console)

      consolePane.setFocusTraversable(true)
    }

    override def update(elapsed: Int): Unit = {
      console.draw(world)
      updatePlayerInfo()

      if (world.player.hitPoints <= 0) {
        changeState(new GameController(WorldBuilder()))
      }
    }

    def updatePlayerInfo(): Unit = {
      import world._

      hitPoints.setText(player.hitPoints.toString)

      damage.setText(player.damage.toString)

      attack.setText(player.attackRating.toString)
      modifier.setText(player.modifier().toString)

      defense.setText(player.defenseRating.toString)
    }

    def handleMouseMove(event: sfxi.MouseEvent): Unit = {
      for (p <- mouseToPoint(event)) {
        console.cursor = Some(p)
      }
    }

    def handleMouseClicked(event: sfxi.MouseEvent): Unit = {
    }

    def handleMouseExit(event: sfxi.MouseEvent): Unit = {
    }

    def mouseToPoint(mouseEvent: sfxi.MouseEvent): Option[Point] = console.pixelToCell(mouseEvent.x, mouseEvent.y)

    def handleKeyPressed(event: sfxi.KeyEvent): Unit = {
      event.consume()
      val code = event.code
      val direction = getDirection(code)
      code match {
        case ESCAPE => exit()
        case TAB => world.player.nextAction()
        case _ => direction.map( p => {
          val text = if (event.controlDown) world.specialAction(p) else world.action(p)
          textArea.appendText(text)
        } )
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


