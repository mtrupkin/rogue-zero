package model

import org.mtrupkin.console.ScreenChar
import org.mtrupkin.core.{Size, Point}

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by mtrupkin on 3/6/2016.
  */
trait Agent {
  var position: Point
  var hitPoints: Int

  // combat stat
  var attackRating: Int

  // situational modifiers
  var modifier: Int = 0

  val sc: ScreenChar
}

case class Monster(
  val name: String,
  var position: Point,
  var hitPoints: Int,
  var attackRating: Int,
  val sc: ScreenChar) extends Agent {
  def attack(player: Player): Option[String] = {
    val damage = Combat.damage(this, player)
    if (damage > 0 ) {
      player.hitPoints -= damage
      Some(s"Monster attacks player for $damage damage.")
    } else None
  }
}

class Player(
  var position: Point,
  var hitPoints: Int,
  var attackRating: Int = 1,
  val sc: ScreenChar) extends Agent {

  var currentActionIndex = 0
  val actions: List[Action] = List(Dash, Burst, Blast)

  def nextAction(): String = {
    currentActionIndex += 1
    if (currentActionIndex >= actions.size) currentActionIndex = 0
    s"\nSpecial action: ${actions(currentActionIndex)}"
  }

  def action(): Action = actions(currentActionIndex)

  def attack(monster: Monster): String = {
    val damage = Math.max(1, Combat.damage(this, monster))

    monster.hitPoints -= damage

    s"Player attacks ${monster.name} for $damage damage."
  }

  sealed trait Action {
    def perform(world: World, direction: Point): Option[String]
  }
  // move 2 squares
  case object Dash extends Action  {
    def perform(world: World, direction: Point): Option[String] = {
      val (canDash, firstText) = world.attackOrMove(direction)
      if (canDash) {
        val (_, secondText) = world.attackOrMove(direction)
        secondText match {
          case Some(text) => Some(s"\nDash\n$text")
          case None => Some(s"\nDash")
        }

      } else firstText
    }
  }

  // attack 3x3 next to player
  case object Burst extends Action  {
    def neighbors(origin: Point, r: Int = 1): Seq[Point] = {
      for {
        x0 <- -r to r
        y0 <- -r to r
        p0 = origin + (x0, y0)
      } yield p0
    }

    def perform(world: World, direction: Point): Option[String] = {
      val origin = position + direction
      val textList = ListBuffer[String]()
      neighbors(origin).foreach(p => {
        world.monsters.find(_.position == p).map(m => textList += attack(m))
      })
      if (textList.isEmpty) None else Some(s"\nBurst\n${textList.mkString("\n")}")
    }
  }
  // attack 3x3 around player
  case object Blast extends Action  {
    def perform(world: World, direction: Point): Option[String] = {
      val textList = ListBuffer[String]()
      position.neighbors().foreach(p => {
        world.monsters.find(_.position == p).map(m => textList += attack(m))
      })
      if (textList.isEmpty) None else Some(s"\nBlast\n${textList.mkString("\n")}")
    }
  }
}

object Combat {
  // chance that a multiple is successful
  val multipleDamageChance = 50

  private def totalHits(attack: Int, modifier: Int): Int = {
    val hits = for {
      i <- 1 to attack
      if ((Random.nextInt(100) + modifier) > multipleDamageChance)
    } yield 1

    hits.sum
  }

  def damage(offense: Agent, defense: Agent): Int = totalHits(offense.attackRating, offense.modifier)
}