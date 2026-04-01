package it.unibo.pps.polyglot.a01a

import scala.util.Random
import it.unibo.pps.util.Sequences.*
import it.unibo.pps.util.Sequences.Sequence.*

trait Point:
  def x: Int
  def y: Int

object Point:
  private val random = Random()

  def apply(x: Int, y: Int): Point =
    PointImpl(x, y)

  def apply(): Point =
    PointImpl(0, 0)

  def apply(size: Int): Point =
    PointImpl(
      random.nextInt(size),
      random.nextInt(size)
    )

  private case class PointImpl(x: Int, y: Int) extends Point


class LogicsImpl(gridSize: Int, boatSize: Int) extends Logics:
  import Logics.Result

  private val MaxFailures = 5
  private val random = Random()

  private var hitCells: Sequence[Point] = Sequence.empty
  private var failures = 0

  private val boatRow: Int = random.nextInt(gridSize)
  private val boatLeftCol: Int = random.nextInt(gridSize - boatSize + 1)

  override def hit(row: Int, col: Int): Result =
    if isBoatCell(row, col) then
      val p = Point(row, col)
      if !hitCells.contains(p) then
        hitCells = hitCells.concat(Sequence(p))
      if sequenceSize(hitCells) == boatSize then Result.WON else Result.HIT
    else
      failures = failures + 1
      if failures == MaxFailures then Result.LOST else Result.MISS

  private def isBoatCell(row: Int, col: Int): Boolean =
    row == boatRow &&
      col >= boatLeftCol &&
      col < boatLeftCol + boatSize

  private def sequenceSize[A](seq: Sequence[A]): Int = seq match
    case Cons(_, tail) => 1 + sequenceSize(tail)
    case Nil()         => 0
