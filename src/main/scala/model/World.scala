package model

import org.mtrupkin.console.ScreenChar
import org.mtrupkin.console.Colors
import org.mtrupkin.core.{Points, Matrix, Size, Point}
import pathfinding.AStar
import rexpaint.RexPaintImage

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by mtrupkin on 2/10/2016.
  */

sealed trait Terrain {
  def name: String
  def sc: ScreenChar
  def move: Boolean
  def areaID: Int
  var explored: Boolean = false
  var marked: Boolean = false
  def display: ScreenChar = if(explored) sc else Terrain.unexplored
}

trait TerrainMap {
  def apply(p: Point): Terrain

}

case class Cell(
                 areaID: Int,
                 name: String,
                 sc: ScreenChar,
                 move: Boolean
) extends Terrain

case class Door(areaID: Int, var open: Boolean = false) extends Terrain {
  def name = if (open) "Opened Door" else "Closed Door"
  def move = open
  def sc: ScreenChar = if (open) ' ' else '+'
}

object Terrain {
  def apply(sc: ScreenChar, areaID: Int): Terrain = {
    sc.c match {
      case ' ' => floor(areaID, sc)
      case '+' => new Door(areaID)
      case _ => wall(areaID, sc)
    }
  }
  def floor(areaID: Int, sc: ScreenChar) = Cell(areaID, "Floor", sc, move = true)
  def wall(areaID: Int, sc: ScreenChar) = Cell(areaID, "Wall", sc, move = false)

  import Colors._
  val unexplored: ScreenChar = ScreenChar(' ', LightGrey, Black)
}

class World(val player: Player) extends TerrainMap {
  val size = Size(40, 20)

  var monsters = List[Monster]()
  val cells = new Matrix[Terrain](size)

  def apply(p: Point): Terrain = cells(p)

  def action(direction: Point): Unit = {
    attackOrMove(direction)
    endAction()
  }

  def specialAction(direction: Point): Unit = {
    player.action().perform(this, direction)
    endAction()
  }

  def endAction(): Unit = {
    monsters = monsters.filter(_.hitPoints > 0)
    monsters.foreach(moveMonster(_))
  }

  // returns true if just a move and no other action
  def attackOrMove(direction: Point): Boolean = {
    val newPosition = player.position + direction
    // either attack or a move
    monsters.find(_.position == newPosition) match {
      case Some(monster) => player.attack(monster); false
      case None => {
        this(newPosition) match {
          case d: Door => {
            d.open = true
            exploreDoor(newPosition)
          }
          case _ =>
        }
        move(newPosition)
      }
    }
  }

  // returns true if moved only
  def move(p: Point): Boolean = {
    val newTerrain = this(p)
    if (!newTerrain.move) return false
    val oldTerrain = this(player.position)
    player.position = p

    var moveOnly = false
    newTerrain match {
      case d @ Door(_, false) => {
        exploreDoor(player.position)
      }
      case _ => moveOnly = true
    }

    if (oldTerrain.areaID != newTerrain.areaID) {
      explore(player.position)
    }

    moveOnly
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

  def exploreDoor(p: Point): Unit = {
    size.neighbors(p).foreach(explore(_))
  }

  def explore(p: Point): Unit = {
    def floodExplore(p: Point, areaID: Int): Unit = {
      val cell = this(p)
      if (cell.marked) return

      cell.marked = true
      cell.explored = true

      if ((cell.areaID == areaID) && cell.move ){
        size.neighbors(p).foreach(n => floodExplore(n, areaID))
      }
    }

    floodExplore(p, this(p).areaID)
    size.foreach(this(_).marked = false)
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
    val image = readRexImage("level-1")
    val map = image.layers(0)
    val metaMap = image.layers(1)
    val start = readPlayerStartPosition(metaMap)
    val player = new Player(start, 5, '@')
    val world = new World(player)
    addTestMonster(world, metaMap)
    map.foreach((p, sc) => world.cells(p) = Terrain(sc, areaID(p, world.size)))

    world.explore(player.position)

    world
  }

  def areaID(p: Point, size: Size): Int = {
    ((p.y / 5) * size.width) + (p.x / 5)
  }

  def addTestMonster(world: World, matrix: Matrix[ScreenChar]): Unit = {
    var acc = ListBuffer[Monster]()
    matrix.foreach((p, sc) => if (sc.c == 'M') {
      val c = ('A' + Random.nextInt(26)).toChar
      acc += Monster(p, 2, c)
    })

    world.monsters = acc.toList
  }
}
