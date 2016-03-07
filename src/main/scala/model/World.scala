package model

import org.mtrupkin.console.ScreenChar
import org.mtrupkin.core.{Points, Matrix, Size, Point}
import pathfinding.AStar
import rexpaint.RexPaintImage

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by mtrupkin on 2/10/2016.
  */
trait Terrain {
  def move: Boolean
}

trait TerrainMap {
  def apply(p: Point): Terrain
}

case class Cell(
  name: String,
  sc: ScreenChar,
  move: Boolean
) extends Terrain

object Cell {
  def apply(sc: ScreenChar): Cell = {
    sc.c match {
      case ' ' => floor(sc)
      case _ => wall(sc)
    }
  }
  def floor(sc: ScreenChar) = Cell("Floor", sc, move = true)
  def wall(sc: ScreenChar) = Cell("Wall", sc, move = false)
}

class World(val player: Player) extends TerrainMap {
  val size = Size(40, 20)

  var monsters = List[Monster]()
  val cells = new Matrix[Cell](size)

  def apply(p: Point): Cell = cells(p)

  def movePlayer(direction: Point): Unit = {
    val newPosition = player.position + direction
    monsters.find(_.position == newPosition) match {
      case Some(monster) => {
        player.attack(monster)
        monsters = monsters.filter(_.hitPoints > 0)
      }
      case None => if (cells(newPosition).move) {
        player.position = newPosition

        monsters.foreach(moveMonster(_))
      }
    }
  }

  val aStar = new AStar(this, size)

  def moveMonster(monster: Monster): Unit = {
    val search = aStar.search(monster, player.position, 15)
    search match {
      case _::path => {
        val position = path.head
        if (player.position == position) {
          monster.attack(player)
          println(s"player hit points: ${player.hitPoints}")
        } else {
          monster.position = path.head
        }
      }
      case nil => return
    }
  }
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
    val image = readRexImage("level-2")
    val map = image.layers(0)
    val metaMap = image.layers(1)
    val start = readPlayerStartPosition(metaMap)
    val player = new Player(start, 1, '@')
    val world = new World(player)
    addTestMonster(world, metaMap)
    map.foreach((p, sc) => world.cells(p) = Cell(sc))
    world
  }

  def addTestMonster(world: World, matrix: Matrix[ScreenChar]): Unit = {
    var acc = ListBuffer[Monster]()
    matrix.foreach((p, sc) => if (sc.c == 'M') {
      val c = ('A' + Random.nextInt(26)).toChar
      acc += Monster(p, 1, c)
    })

    world.monsters = acc.toList
  }
}
