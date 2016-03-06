package model

import org.mtrupkin.console.ScreenChar
import org.mtrupkin.core.{Points, Matrix, Size, Point}
import rexpaint.RexPaintImage

/**
  * Created by mtrupkin on 2/10/2016.
  */
case class Cell(id: Int = 0, name: String = "floor", sc: ScreenChar)

class World(val player: Player) {

  val size = Size(80, 40)
  val cells = new Matrix[Cell](size)

  def apply(p: Point): Cell = cells(p)
}

object World {
  protected def readRexImage(name: String): RexPaintImage = {
    val is = getClass.getResourceAsStream(s"/rex/$name.xp")
    RexPaintImage.read(name, is)
  }

  protected def readPlayerStartPosition(matrix: Matrix[ScreenChar]): Point = {
    var start = Points.Origin
    matrix.foreach((p, sc) => if (sc.c == 'S') {
      matrix(p) = sc.copy(c = ' ')
      start = p
    })

    start
  }

  def apply(): World = {
    val image = readRexImage("test1")
    val layer = image.layers.head
    val start = readPlayerStartPosition(layer)
    val player = new Player(start)
    val world = new World(player)
    layer.foreach((p, sc) => world.cells(p) = Cell(sc = sc))
    world
  }
}
