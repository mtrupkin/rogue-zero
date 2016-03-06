package app

import javafx.application.Application
import javafx.stage.Stage

import controller.Controller
import model.World

class ConsoleApp extends Application {
	override def start(primaryStage: Stage) {
		primaryStage.setTitle("Rogue Zero")

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