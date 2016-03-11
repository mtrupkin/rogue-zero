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

  private def reverseRows(m: Matrix[ScreenChar]): Matrix[ScreenChar] = {
    def reverseRow[T](m: Matrix[ScreenChar], n: Matrix[ScreenChar], rowIndex: Int): Unit = {
      import m.size
      val row = for {
        i <- Range(0, size.width)
      } yield m(i, rowIndex)
      val reversed = row.reverse
      for {
        i <- Range(0, size.width)
      } n.update((i, rowIndex), reversed(i))
    }

    val n = new Matrix[ScreenChar](m.size)
    Range(0, m.size.height).foreach(reverseRow(m, n, _))
    n
  }

  private def reverseColumns(m: Matrix[ScreenChar]): Matrix[ScreenChar] = {
    def reverseColumn(m: Matrix[ScreenChar], n: Matrix[ScreenChar], columnIndex: Int): Unit = {
      import m.size
      val column = for {
        i <- Range(0, size.height)
      } yield m(columnIndex, i)
      val reversed = column.reverse
      for {
        i <- Range(0, size.width)
      } n.update((columnIndex, i), reversed(i))
    }

    val n = new Matrix[ScreenChar](m.size)
    Range(0, m.size.width).foreach(reverseColumn(m, n, _))
    n
  }

  def rotateClockwise(m: Matrix[ScreenChar]): Matrix[ScreenChar] = {
    val n = transpose(m)
    reverseRows(n)
  }

  def rotateCounterClockwise(m: Matrix[ScreenChar]): Matrix[ScreenChar] = {
    val n = transpose(m)
    reverseColumns(n)
  }

  def rotate180(m: Matrix[ScreenChar]): Matrix[ScreenChar] = {
    val n = reverseRows(m)
    reverseColumns(n)
  }

  def flipX(m: Matrix[ScreenChar]): Matrix[ScreenChar] = {
    reverseColumns(m)
  }

  def flipY(m: Matrix[ScreenChar]): Matrix[ScreenChar] = {
    reverseRows(m)
  }
}
