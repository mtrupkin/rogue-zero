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

  // combat stats
  var damage: Int = 1
  var attackRating: Int = 1

  // situational modifiers
  def modifier(): Int = 0
  var defenseRating: Int = 1

  val sc: ScreenChar
}

case class Monster(
  val name: String,
  var position: Point,
  var hitPoints: Int,
  val sc: ScreenChar) extends Agent {
  def attack(player: Player): Option[String] = {
    val bonusDamage = Combat.bonusDamage(this, player)
    if (bonusDamage > 0 ) {
      player.hitPoints -= bonusDamage
      Some(s"Player damaged: $bonusDamage")
    } else None
  }
}

class Player(
  var position: Point,
  var hitPoints: Int,
  val sc: ScreenChar) extends Agent {

  var currentActionIndex = 0
  val actions: List[Action] = List(Dash, Burst, Blast)

  def nextAction(): Unit = {
    currentActionIndex += 1
    if (currentActionIndex >= actions.size) currentActionIndex = 0
    println(actions(currentActionIndex))
  }

  def action(): Action = actions(currentActionIndex)

  def attack(monster: Monster): String = {
    val bonusDamage = Combat.bonusDamage(this, monster)
    val totalDamage = damage + bonusDamage

    monster.hitPoints -= totalDamage

    s"Attack ${monster.name}: $totalDamage"
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
        secondText
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
      if (textList.isEmpty) None else Some(textList.mkString("\n"))
    }
  }
  // attack 3x3 around player
  case object Blast extends Action  {
    def perform(world: World, direction: Point): Option[String] = {
      val textList = ListBuffer[String]()
      position.neighbors().foreach(p => {
        world.monsters.find(_.position == p).map(m => textList += attack(m))
      })
      if (textList.isEmpty) None else Some(textList.mkString("\n"))
    }
  }
}

object Combat {
  // chance that a multiple is successful
  val multipleDamageChance = 50

  private def bonusDamage(damage: Int, multiplier: Int, modifier: Int): Int = {
    val bonuses = for {
      i <- 1 to multiplier
      if (Random.nextInt(100) + modifier > multipleDamageChance)
    } yield damage

    bonuses.sum
  }

  def bonusDamage(offense: Agent, defense: Agent): Int = {
    val multiplier = Math.max(offense.attackRating - defense.defenseRating, 1)
    bonusDamage(offense.damage, multiplier, offense.modifier())
  }

}