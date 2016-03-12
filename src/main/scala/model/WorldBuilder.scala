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
  def scale(p: Point, s: Int): Point = Point(p.x * s, p.y * s)

  def toWorld(areaPosition: Point, p: Point): Point = scale(areaPosition, 5) + p

  def apply(): World = {
    val monsters = new ListBuffer[Monster]()

    val startArea = Area.randomStartArea()
    val areaMap = new AreaMap(startArea)
    val (startMap, startPlayerPosition) = readStartArea(startArea)

    val player = new Player(toWorld(areaMap.startAreaPosition, startPlayerPosition), 5, '@')
    val world = new World(player)
    world.size.foreach(world.cells(_) = Terrain.inBoundsWall)

    areaMap.areas.size.foreach(p => {
      if(Option(areaMap.areas(p)).isDefined) {
        writeArea(readArea(areaMap.areas(p), p, monsters), scale(p, 5), world)
      }
    })

    world.monsters = monsters.toList

    world.explore(player.position)

    world
  }

  def writeArea(map: Matrix[Terrain], p0: Point, world: World): Unit = {
    map.foreach((p, cell) => world.cells(p+p0) = cell)
  }

  def readStartArea(area: Area): (Matrix[Terrain], Point) = {
    val (terrainMap, metaMap) = parseArea(area)
    var start = Points.Origin
    metaMap.foreach((p, sc) => if (sc.c == 'S') start = p)
    (terrainMap, start)
  }

  def readArea(area: Area, p0: Point, monsters: ListBuffer[Monster]): Matrix[Terrain] = {
    val (terrainMap, metaMap) = parseArea(area)
    metaMap.foreach((p, sc) => if (sc.c == 'M') monsters += Monster("Monster", toWorld(p0, p), 2, 'M'))
    terrainMap
  }

  var areaID: Int = 0
  def parseArea(area: Area): (Matrix[Terrain], Matrix[ScreenChar]) = {
    areaID += 1
    val image = area.image
    val map = image.layers(0)
    val metaMap = image.layers(1)

    val terrainMap = new Matrix[Terrain](map.size)
    map.foreach((p, sc) => terrainMap(p) = Terrain(sc, areaID))

    (terrainMap, metaMap)
  }
}

class AreaMap(startArea: Area) {
  val size = Size(8, 4)
  val areas = new Matrix[Area](size)
  def random2(): Point = {
    import Random.nextInt; import size._
    Point(nextInt(width), nextInt(height))
  }

  def randomStartPosition(): Point = {
    import Random.nextInt; import size._
    Point(nextInt(width-2)+1, nextInt(height-2)+1)
  }

  val startAreaPosition = randomStartPosition()
  areas(startAreaPosition) = startArea

  nextArea(startArea.outputs.map(p => (startAreaPosition, p)))

  def nextArea(acc: List[(Point, Point)]): Unit = {
    acc match {
      case head::tail => {
        val (currentPosition, output) = head
        val nextPosition = currentPosition + output
        if (size.in(nextPosition) && Option(areas(nextPosition)).isEmpty) {
          val area = Area.randomArea(output)
          areas(nextPosition) = area
          val nextOutputs = area.outputs.map(p=>(nextPosition,p))
          nextArea(nextOutputs ++ tail)
        } else {
          nextArea(tail)
        }
      }
      case Nil =>
    }
  }

  def debug(): Unit = {
    println(s"start: $startAreaPosition")

    size.foreach(p => {
      Option(areas(p)).map(a => {
        println(p)
        a.debug()
      })
    })
  }

}

case class Area(image: RexPaintImage, input: Point, outputs: List[Point]) {
  def debug():Unit = {
    println(s"input: $input outputs: $outputs")
    val layer = image.layers(0)
    for {
      y <- (0 until image.size.height)
    } {
      for {
        x <- (0 until image.size.width)
      } print(layer((x,y)).c)
      println
    }
    println()
  }
}

object Area {
  private def readRexImage(name: String): RexPaintImage = {
    val is = getClass.getResourceAsStream(s"/rex/$name.xp")
    RexPaintImage.read(name, is)
  }

  import Points._
  val startArea: Area = Area(readRexImage("start-0"), Up, List(Left, Right, Up))
  val baseAreas: List[Area] = {
    val a1 = Area(readRexImage("area-1"), Up, List(Up))
    val a2 = Area(readRexImage("area-2"), Up, List(Right))
    val a3 = Area(readRexImage("area-3"), Up, Nil)

    List(a1, a2, a3)
  }

  val areas: List[Area] = baseAreas.flatMap(allTransforms(_))
  val startAreas: List[Area] = allTransforms(startArea)

//  startAreas.foreach(_.debug())
//  areas.foreach(_.debug())

  private def random(areas: List[Area]): Area = areas(Random.nextInt(areas.size))

  def randomStartArea(): Area = random(startAreas)

  def randomArea(inputDirection: Point): Area = random(areas.filter(a=>a.input==inputDirection))

  //
  def allTransforms(area: Area): List[Area] = {
    val transforms: List[Area=>Area] = List(a=>a, rotate180, rotateCounterClockwise, rotateClockwise, flipX, flipY)
    transforms.map(_(area))
  }

  def rotateCounterClockwise(area: Area): Area = transform(area, MatrixUtil.rotateCounterClockwise, p => p.copy(x = p.y, y = -p.x))
  def rotateClockwise(area: Area): Area = transform(area, MatrixUtil.rotateClockwise, p => p.copy(x = -p.y, y = p.x))
  def rotate180(area: Area): Area = transform(area, MatrixUtil.rotate180, p => p.copy(x = -p.x, y = -p.y))

  def flipX(area: Area): Area = transform(area, MatrixUtil.flipX, p => p.copy(y = -p.y))
  def flipY(area: Area): Area = transform(area, MatrixUtil.flipY, p => p.copy(x = -p.x))

  private def transform(area: Area,
    matrixTransform: Matrix[ScreenChar] => Matrix[ScreenChar],
    pointTransform: Point => Point): Area = {

    val map = matrixTransform(area.image.layers(0))
    val metaMap = matrixTransform(area.image.layers(1))

    val input = pointTransform(area.input)
    val outputs = area.outputs.map(pointTransform(_))

    val image = area.image.copy(layers=List(map, metaMap))

    Area(image, input, outputs)
  }
}
