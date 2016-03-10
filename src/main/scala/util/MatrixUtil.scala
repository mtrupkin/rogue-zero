package util

import org.mtrupkin.console.ScreenChar
import org.mtrupkin.core.Matrix

/**
  * Created by mtrupkin on 3/9/2016.
  */
object MatrixUtil {
  private def transpose(m: Matrix[ScreenChar]): Matrix[ScreenChar] = {
    import m.size

    val transposed = new Matrix[ScreenChar](size)
    size.foreach(p => transposed((p.y, p.x)) = m(p))
    transposed
  }

  private def reverseRows(m: Matrix[ScreenChar]): Unit = {
    def reverseRow[T](m: Matrix[ScreenChar], rowIndex: Int): Unit = {
      import m.size
      val row = for {
        i <- Range(0, size.width)
      } yield m(i, rowIndex)
      val reversed = row.reverse
      for {
        i <- Range(0, size.width)
      } m.update((i, rowIndex), reversed(i))
    }

    Range(0, m.size.height).foreach(reverseRow(m, _))
  }

  private def reverseColumns(m: Matrix[ScreenChar]): Unit = {
    def reverseColumn(m: Matrix[ScreenChar], columnIndex: Int): Unit = {
      import m.size
      val column = for {
        i <- Range(0, size.height)
      } yield m(columnIndex, i)
      val reversed = column.reverse
      for {
        i <- Range(0, size.width)
      } m.update((columnIndex, i), reversed(i))
    }

    Range(0, m.size.width).foreach(reverseColumn(m, _))
  }

  def rotateClockwise(m: Matrix[ScreenChar]): Matrix[ScreenChar] = {
    val n = transpose(m)
    reverseRows(n)
    n
  }

  def rotateCounterClockwise(m: Matrix[ScreenChar]): Matrix[ScreenChar] = {
    val n = transpose(m)
    reverseColumns(n)
    n
  }

  def rotate180(m: Matrix[ScreenChar]): Matrix[ScreenChar] = {
    val n = new Matrix[ScreenChar](m.size)
    reverseRows(n)
    reverseColumns(n)
    n
  }

  def flipX(m: Matrix[ScreenChar]): Matrix[ScreenChar] = {
    reverseColumns(m)
    m
  }

  def flipY(m: Matrix[ScreenChar]): Matrix[ScreenChar] = {
    reverseRows(m)
    m
  }
}
