import scala.reflect.ClassTag

class Matrix[T : ClassTag](val array: Array[Array[T]])(using num: Numeric[T]):
  
  val rows: Int = array.length
  
  val cols: Int = array(0).length

  require(array.forall(_.length == cols), "All rows of matrix should have the same number of elements!")

  def size: (Int, Int) = (rows, cols)

  def isSquare: Boolean = rows == cols

  // Access to matrix elements
  
  def apply(i: Int, j: Int): T = array(i)(j)

  def apply(index: (Int, Int)): T = array(index._1)(index._2)

  def apply(rs: Seq[Int], сs: Seq[Int]): Matrix[T] =
    val a = Array.tabulate(rows, cols) {
      (i, j) => array(i)(j)
    }
    new Matrix[T](a)

  // Matrix operation

  def t: Matrix[T] = new Matrix[T](array.transpose)

  def unary_- : Matrix[T] = ???

  def +(that: Matrix[T]): Matrix[T] =
    require(rows == that.rows && cols == that.cols, "Matrices must have the same number of rows and columns!")
    val a = Array.tabulate(rows, cols) {
      (i, j) => num.plus(array(i)(j), that.array(i)(j))
    }
    new Matrix[T](a)

  def -(that: Matrix[T]): Matrix[T] = 
    require(rows == that.rows && cols == that.cols, "Matrices must have the same number of rows and columns!")
    val a = Array.tabulate(rows, cols) {
      (i, j) => num.minus(array(i)(j), that.array(i)(j))
    }
    new Matrix[T](a)

  def *(scalar: T): Matrix[T] = 
    val a = Array.tabulate(rows, cols) {
      (i, j) => num.times(scalar, array(i)(j))
    }
    new Matrix(a)

  def *(that: Matrix[T]): Matrix[T] =
    require(cols == that.rows, "Matrices must have the same number of rows and columns!")
    val a = Array.tabulate(rows, that.cols) {
      (i, j) => 
        var x: T = num.fromInt(0)
        for 
          k <- 0 until cols
        do
          x = num.plus(x, num.times(array(i)(k), that.array(k)(j)))
        x
    }
    new Matrix[T](a)

  override def toString: String = 
    val maxWidth: Int = array.flatten.map(_.toString.length).max
    val sb = new StringBuilder()
    for
      i <- 0 until rows
      j <- 0 until cols
    do
      sb ++= array(i)(j).toString.padTo(maxWidth, ' ')
      if j == cols - 1 then sb += '\n' else sb += ' '
    sb.toString  
      
        

object Matrix:
  
  def main(args: Array[String]): Unit =
    val m = new Matrix[Int](Array(Array(1, 2, 3), Array(4, 5, 6)))
    println(s"Matrix m has ${m.rows} rows and ${m.cols} columns")
    println(m)
    val m1 = Matrix(2, 2, {(x, y) => x + y})
    println(m1)
    val m2 = m(1 to 2, 1 to 2)
    println(m2)
    val a = Array(1, 2, 3)
    val b = new Matrix[Int](Array(a))
    import ScalarOps._
    val c = m * 5
    println(c)
    val d = Matrix.fill(4, 4, 4)
    println(d)
    val e = Matrix.fill(4, 4, 3)
    println(e - d)
    val f = new Matrix[Int](Array(
      Array(5, 3),
      Array(-2, 4),
      Array(-1, 0)
    ))
    val g = new Matrix[Int](Array(
      Array(2, 0, 1, 3),
      Array(4, 5, 2, 6)
    ))
    println(f * g)
  
  def apply[T: Numeric : ClassTag](rows: Int, cols: Int, f: (Int, Int) => T): Matrix[T] =
    val a = Array.tabulate(rows, cols)(f)
    new Matrix[T](a)

  def fill[T : Numeric : ClassTag](rows: Int, cols: Int, v: T): Matrix[T] =
    val a = Array.fill(rows, cols)(v)
    new Matrix[T](a)