package model

import org.mtrupkin.console.ScreenChar
import org.mtrupkin.core.Point

import scala.util.Random

/**
  * Created by mtrupkin on 3/6/2016.
  */
trait Agent {
  var position: Point
  var hitPoints: Int

  // combat stats
  var damage: Int = 1
  var attack: Int = 1
  def modifier(): Int = 1
  var defense: Int = 1

  val sc: ScreenChar
}

case class Monster(
  var position: Point,
  var hitPoints: Int,
  val sc: ScreenChar) extends Agent {
  def attack(player: Player): Unit = {
    val bonusDamage = Combat.bonusDamage(this, player)

    player.hitPoints -= bonusDamage
  }
}

class Player(
  var position: Point,
  var hitPoints: Int,
  val sc: ScreenChar) extends Agent {

  def attack(monster: Monster): Unit = {
    val bonusDamage = Combat.bonusDamage(this, monster)
    val totalDamage = damage + bonusDamage
    println(s"attack damage: $totalDamage")

    monster.hitPoints -= totalDamage
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
    val multiplier = Math.max(offense.attack - defense.defense, 1)
    bonusDamage(offense.damage, multiplier, offense.modifier())
  }

}