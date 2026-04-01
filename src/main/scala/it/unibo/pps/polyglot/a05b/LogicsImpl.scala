package it.unibo.pps.polyglot.a05b

import it.unibo.pps.polyglot.a05b.Logics

trait Point:
  def x: Int
  def y: Int

object Point:

  import scala.util.Random

  private val random = Random()

  def apply(x: Int, y: Int): Point = PointImpl(x, y)
  def apply(): Point = PointImpl(0, 0)
  def apply(size: Int): Point = PointImpl(random.nextInt(size - 2) + 1, random.nextInt(size - 2) + 1)

  private case class PointImpl(x: Int, y: Int) extends Point

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private val initialPoint = Point(size)

  private var tickCount = 0

  require(size >= 3, "Grid size must be at least 3")

  override def tick(): Unit = tickCount = tickCount + 1

  override def isOver: Boolean =
      initialPoint.y - tickCount < 0 ||
      initialPoint.y + tickCount >= size ||
      initialPoint.x - tickCount < 0 ||
      initialPoint.x + tickCount >= size

  override def hasElement(x: Int, y: Int): Boolean =
    (x == initialPoint.x && math.abs(y - initialPoint.y) <= tickCount) ||
    (y == initialPoint.y && math.abs(x - initialPoint.x) <= tickCount) ||
    (x - y == initialPoint.x - initialPoint.y && math.abs(x - initialPoint.x) <= tickCount) ||
    (x + y == initialPoint.x + initialPoint.y && math.abs(x - initialPoint.x) <= tickCount)
