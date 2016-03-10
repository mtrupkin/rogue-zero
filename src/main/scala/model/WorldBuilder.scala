package model


import org.mtrupkin.console.ScreenChar
import org.mtrupkin.core.{Size, Points, Point, Matrix}
import rexpaint.RexPaintImage
import util.MatrixUtil

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by mtrupkin on 3/9/2016.
  */
object WorldBuilder {
  private def readRexImage(name: String): RexPaintImage = {
    val is = getClass.getResourceAsStream(s"/rex/$name.xp")
    RexPaintImage.read(name, is)
  }

  def apply(): World = {
    val monsters = new ListBuffer[Monster]()

    import MatrixUtil._

    val a1 = readArea("area-2", flipY, monsters = monsters)
    val a2 = readArea("area-1", monsters = monsters)
    val (startMap, startPosition) = readStartArea("start-0")
    val a4 = readArea("area-2", flipX, monsters = monsters)
    val a5 = readArea("area-2", m => flipX(rotateClockwise(m)), monsters = monsters)
    val a6 = readArea("area-3", monsters = monsters)

    val player = new Player(startPosition + (0,5), 5, '@')
    val world = new World(player)
    world.size.foreach(world.cells(_) = Terrain.wall(0, '!'))

    writeArea(a1, (0,0), world)
    writeArea(a2, (5,0), world)
    writeArea(startMap, (0,5), world)
    writeArea(a4, (0, 10), world)
    writeArea(a5, (5, 5), world)
    writeArea(a6, (5, 10), world)

    world.explore(player.position)

    world
  }

  def writeArea(map: Matrix[Terrain], p0: Point, world: World): Unit = {
    map.foreach((p, cell) => world.cells(p+p0) = cell)
  }

  var areaID: Int = 0

  def readStartArea(name: String, transform: (Matrix[ScreenChar])=>Matrix[ScreenChar] = m => m): (Matrix[Terrain], Point) = {
    val (terrainMap, metaMap) = readMetaMap(name, transform)
    var start = Points.Origin

    metaMap.foreach((p, sc) => if (sc.c == 'S') start = p)
    (terrainMap, start)
  }

  def readArea(name: String, transform: (Matrix[ScreenChar])=>Matrix[ScreenChar] = m => m, monsters: ListBuffer[Monster]): Matrix[Terrain] = {
    val (terrainMap, metaMap) = readMetaMap(name, transform)

    metaMap.foreach((p, sc) => if (sc.c == 'M') monsters += Monster(p, 2, 'M'))

    terrainMap
  }

  def readMetaMap(name: String, transform: (Matrix[ScreenChar])=>Matrix[ScreenChar]): (Matrix[Terrain], Matrix[ScreenChar]) = {
    areaID += 1

    val image = readRexImage(name)
    val map = transform(image.layers(0))
    val metaMap = transform(image.layers(1))

    val terrainMap = new Matrix[Terrain](map.size)
    map.foreach((p, sc) => terrainMap(p) = Terrain(sc, areaID))

    (terrainMap, metaMap)
  }


  def inputDirection(outputDirection: Point): Point = (outputDirection.x * -1, outputDirection.y * -1)

  def getInputArea(outputDirection: Point): Matrix[Terrain] = ???
}
