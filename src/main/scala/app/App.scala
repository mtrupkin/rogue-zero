package app

import java.awt.{Font, GraphicsEnvironment}
import javafx.application.Application
import javafx.stage.Stage

import controller.Controller
import model.World


class ConsoleApp extends Application {
	override def start(primaryStage: Stage) {
		val ge = GraphicsEnvironment.getLocalGraphicsEnvironment()
    val is = getClass.getResourceAsStream(s"/fonts/RobotoMono-Regular.ttf")
		ge.registerFont(Font.createFont(Font.TRUETYPE_FONT, is))

		primaryStage.setTitle("Rogue 0")

		object Controller extends Controller {
			lazy val initialState: ControllerState = new GameController(World())
			lazy val stage = primaryStage
		}

		Controller.stage.show()
	}
}


object ConsoleApp extends App {
	Application.launch(classOf[ConsoleApp], args: _*)
}