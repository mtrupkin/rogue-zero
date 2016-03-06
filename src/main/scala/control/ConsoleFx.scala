package control

import java.lang.Math._
import javafx.scene.control.Label
import javafx.scene.layout.{StackPane, Pane}
import javafx.scene.paint.Color
import javafx.scene.text.{FontWeight, Font}
import model.{Cell, World}
import org.mtrupkin.core.{Point, Matrix, Size}

/**
  * Created by mtrupkin on 2/10/2016.
  */
class ConsoleFx(val size: Size = Size(80, 40)) extends Pane {
  setStyle("-fx-background-color: black;")
  val offsetX, offsetY = 1
  val fontSize: Int = 21
  val font = Font.font("Consolas", FontWeight.NORMAL, fontSize)
  val cellSize = ConsoleFx.charBounds(font)

  val labels = new Matrix[Label](size)
  val (conWidth, conHeight) = cellToPixel(Point(size.width, size.height))
  val (sizeX, sizeY) = (conWidth + offsetX * 2, conHeight + offsetY * 2)
  setPrefSize(sizeX, sizeY)
  setMinSize(sizeX, sizeY)

  size.foreach(init)

  def init(p: Point): Unit = {
    val l = new Label()
    l.setTextFill(Color.WHITE)
    l.setFont(font)

    labels(p) = l

    val (px, py) = cellToPixel(p.x, p.y)
    l.relocate(px, py)
    getChildren.add(l)
  }

  // draw to window
  def draw(world: World): Unit = {
    size.foreach(p => draw(p, world(p)))
  }

  def draw(p: Point, cell: Cell): Unit = {
    labels(p).setText(cell.sc)
  }

  def cellToPixel(p: Point): (Double, Double) = {
    val (width, height) = Size.SizeToTuple(cellSize)

    (p.x * width + offsetX, p.y * height + offsetY)
  }

  def pixelToCell(x: Double, y: Double): Option[Point] = {
    def floor(d: Double): Int = { Math.floor(d).toInt }

    val (width, height) = Size.SizeToTuple(cellSize)
    val c = Point(floor((x-offsetX) / width), floor((y-offsetY) / height))
    if (size.in(c)) Some(c) else None
  }
}

object ConsoleFx {
  def charBounds(f: Font): (Double, Double) = {
    val fl = com.sun.javafx.tk.Toolkit.getToolkit.getFontLoader

    val metrics = fl.getFontMetrics(f)
    val fontWidth = fl.computeStringWidth(" ", f)
    (floor(fontWidth), floor(metrics.getLineHeight))
  }
}