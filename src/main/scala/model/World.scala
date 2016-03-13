package model

import org.mtrupkin.console.{RGB, ScreenChar, Colors}
import org.mtrupkin.core.{Matrix, Size, Point}
import pathfinding.AStar

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
  def action(world: World): Option[String] = None
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
  val doorColor = RGB(50, 50, 50)
  val openScreenChar = ScreenChar('.', Colors.White, Terrain.floorColor)
  val closedScreenChar = ScreenChar('+', Colors.White, doorColor)

  def name = if (open) "Opened Door" else "Closed Door"
  def move = open
  def sc: ScreenChar = if (open) openScreenChar else closedScreenChar
}

case class Treasure(areaID: Int) extends Terrain {
  var looted: Boolean = false
  val unlootedScreenChar = ScreenChar('*', Colors.Yellow, Terrain.floorColor)
  val lootedScreenChar = ScreenChar('.', Colors.White, Terrain.floorColor)

  def name = "Treasure"
  def move = true

  override def sc: ScreenChar = if (looted) lootedScreenChar else unlootedScreenChar

  override def action(world: World): Option[String] = {
    if (looted) return None

    looted = true
    Random.nextInt(100) match {
      case x if 0 until 20 contains x => {
        world.player.hitPoints += 2
        Some("Healing potion.")
      }
      case x if 20 until 40 contains x => {
        world.player.attackRating += 1
        Some("Weapon damage +1.")
      }
      case x if 40 until 60 contains x => {
        world.player.modifier += 5
        Some("Weapon to hit +1")
      }
      case _ => None
    }
  }
}


object Terrain {
  val floorColor = RGB(70, 70, 70)

  def apply(sc: ScreenChar, areaID: Int): Terrain = {
    sc.c match {
      case ' ' => floor(areaID, sc)
      case '+' => new Door(areaID)
      case _ => wall(areaID, sc)
    }
  }

  def floor(areaID: Int, sc: ScreenChar) = Cell(areaID, "Floor", sc.copy(c = '.'), move = true)
  def wall(areaID: Int, sc: ScreenChar) = Cell(areaID, "Wall", sc, move = false)
  def stairsDown(areaID: Int) = new Cell(areaID, "Stairs Down", ScreenChar('>', Colors.White, floorColor), move = true) {
    override def action(world: World): Option[String] = {
      WorldBuilder.newWorld(world)
      Some(s"Descending to level ${world.level}")
    }
  }

  val outOfBoundsWall: Cell = wall(-1, ScreenChar('#', Colors.White, floorColor))
  //val inBoundsWall: Cell = wall(-2, ScreenChar('#', Colors.White, floorColor))
  def inBoundsWall(): Cell = wall(0, ScreenChar('#', Colors.White, floorColor))

  import Colors._
  val unexplored: ScreenChar = ScreenChar(' ', LightGrey, Black)
}

class World(val player: Player) extends TerrainMap {
  var level = 0
  val size = Size(40, 20)
  val cells = new Matrix[Terrain](size)
  var monsters = List[Monster]()

  def apply(p: Point): Terrain = if(size.in(p)) cells(p) else Terrain.outOfBoundsWall

  def action(direction: Point): String = {
    val moveMonsters = triggersMonsterMove(player.position + direction)
    val (_, playerText) = attackOrMove(direction)

    val gameText = if (moveMonsters) endAction() else None

    val s1 = playerText match {
      case Some(t) => s"\n$t"
      case None => ""
    }
    val s2 = gameText match {
      case Some(t) => s"\n$t"
      case None => ""
    }
    s"$s1$s2"
  }

  def specialAction(direction: Point): String = {
    val playerText = player.action().perform(this, direction)
    val gameText = endAction()

    val s1 = playerText match {
      case Some(t) => s"\n$t"
      case None => ""
    }
    val s2 = gameText match {
      case Some(t) => s"\n$t"
      case None => ""
    }
    s"$s1$s2"
  }

  def endAction(): Option[String] = {
    val text = ListBuffer[String]()
    monsters.foreach(m => if (m.hitPoints <= 0) text.append(s"${m.name} died."))
    monsters = monsters.filter(_.hitPoints > 0)
    monsters.foreach(moveMonster(_).map(t => text.append(t)))

    this(player.position).action(this).map(t => text.append(t))

    if (text.isEmpty) None else Some(text.mkString("\n"))
  }

  // returns true if just a move and no other action
  def attackOrMove(direction: Point): (Boolean, Option[String]) = {
    val newPosition = player.position + direction
    // either attack or a move
    monsters.find(_.position == newPosition) match {
      case Some(monster) => (false, Some(player.attack(monster)))
      case None => {
        this(newPosition) match {
          case d: Door => {
            d.open = true
            exploreDoor(newPosition)
          }
          case _ =>
        }
        (move(newPosition), None)
      }
    }
  }

  def triggersMonsterMove(p: Point): Boolean = {
    val newTerrain = this(p)
    newTerrain match {
      case Door(_, open) => open
      case Cell(_, _, _, move) => move
      case c: Treasure => true
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

  def moveMonster(monster: Monster): Option[String] = {
    val search = aStar.search(monster, player.position, 15)
    search match {
      case _::path => {
        val position = path.head
        if (player.position == position) {
          monster.attack(player)
        } else {
          if (!monsters.exists(m => m.position == path.head)) {
            monster.position = path.head
          }
          None
        }
      }
      case nil => None
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

