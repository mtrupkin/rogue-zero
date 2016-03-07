package control


import javafx.scene.control.Label
import javafx.scene.layout.{StackPane, Pane}
import javafx.scene.paint.Color
import javafx.scene.text.{FontWeight, Font}
import model.{Cell, World}
import org.mtrupkin.console.ScreenChar
import org.mtrupkin.core.{Point, Matrix, Size}
import collection.JavaConversions._
import scalafx.scene.paint.Paint

/**
  * Created by mtrupkin on 2/10/2016.
  */
class ConsoleFx(val size: Size = Size(40, 20)) extends Pane {
  val fontSize: Int = 38 // 32
  //val is = getClass.getResourceAsStream("/fonts/RobotoMono-Regular.ttf")
  val font = Font.loadFont(getClass.getResource("/fonts/RobotoMono-Regular.ttf").toExternalForm, 35)
  //val font = java.awt.Font.font("Consolas", FontWeight.NORMAL, fontSize)
  val cellSize = ConsoleFx.charBounds(font)

  val labels = new Matrix[Label](size)
  val (conWidth, conHeight) = cellToPixel(Point(size.width, size.height))
  val (sizeX, sizeY) = (conWidth, conHeight)
  setPrefSize(sizeX, sizeY)
  setMinSize(sizeX, sizeY)

  size.foreach(init)

  def init(p: Point): Unit = {
    val l = new Label()
    l.setTextFill(Color.WHITE)
    l.setFont(font)
    l.setStyle("-fx-border-color: white;")
    labels(p) = l


    val (px, py) = cellToPixel(p.x, p.y)
    l.relocate(px, py)
    getChildren.add(l)
  }

  // draw to window
  def draw(world: World): Unit = {
    size.foreach(p => draw(p, world(p).sc))

    val entities = world.player :: world.monsters
    entities.foreach(e => draw(e.position, e.sc))
  }

  def draw(p: Point, sc: ScreenChar): Unit = {
    val l = labels(p)

    {
      import sc.fg._
      l.setTextFill(Color.rgb(r, g, b))
    }

    {
      import sc.bg._
      val r0 = Integer.toHexString(r)
      val g0 = Integer.toHexString(g)
      val b0 = Integer.toHexString(b)
      l.setStyle(s"-fx-background-color: #$r0$g0$b0")
    }

    labels(p).setText(sc)
  }


  def cellToPixel(p: Point): (Double, Double) = {
    val (width, height) = cellSize
    val (borderWidth, borderHeight) = (2, 2)

    (p.x * width + borderWidth, p.y * height + borderHeight)
  }

//  def pixelToCell(x: Double, y: Double): Option[Point] = {
//    def floor(d: Double): Int = { Math.floor(d).toInt }
//
//    val (width, height) = cellSize
//    val c = Point(floor(x / width), floor(y / height))
//    if (size.in(c)) Some(c) else None
//  }
}

object ConsoleFx {
  def charBounds(f: Font): (Double, Double) = {
    import java.lang.Math.round
    val fl = com.sun.javafx.tk.Toolkit.getToolkit.getFontLoader

    val fontWidth = fl.computeStringWidth(" ", f)
    val metrics = fl.getFontMetrics(f)
    println(metrics)
    println(s"fontWidth: $fontWidth")
    (Math.floor(fontWidth), Math.floor(metrics.getLineHeight+2))
  }
}