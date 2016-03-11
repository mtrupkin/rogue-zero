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
  def sc: ScreenChar = if (open) '.' else '+'
}

object Terrain {
  def apply(sc: ScreenChar, areaID: Int): Terrain = {
    if (sc == null) {
      println("problem")
    }

    sc.c match {
      case ' ' => floor(areaID, sc)
      case '+' => new Door(areaID)
      case _ => wall(areaID, sc)
    }
  }
  def floor(areaID: Int, sc: ScreenChar) = Cell(areaID, "Floor", sc.copy(c = '.'), move = true)
  def wall(areaID: Int, sc: ScreenChar) = Cell(areaID, "Wall", sc, move = false)


  import Colors._
  val unexplored: ScreenChar = ScreenChar(' ', LightGrey, Black)
}

class World(val player: Player) extends TerrainMap {
  val size = Size(40, 20)
  val cells = new Matrix[Terrain](size)
  var monsters = List[Monster]()

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

