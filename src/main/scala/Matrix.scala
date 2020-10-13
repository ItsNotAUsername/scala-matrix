import scala.reflect.ClassTag
import scala.annotation.tailrec

class Matrix[T: ClassTag] (val data: Array[Array[T]])(using num: Numeric[T]):

  require(data.length > 0,
    "The number of rows in the input array must be greater than zero")

  require(data(0).length > 0 && data.forall(_.length == data(0).length),
    "All rows of the input array must contain the same non-zero number of elements")

  val rows: Int = data.length

  val cols: Int = data(0).length

  def size: (Int, Int) = (rows, cols)

  def isSquare: Boolean = rows == cols

  // Access to matrix elements

  private def nonValidRow(row: Int): Boolean =
    row < 0 || row >= rows

  private def nonValidCol(col: Int): Boolean =
    col < 0 || col >= cols

  def apply(row: Int, col: Int): T =
    if nonValidRow(row) || nonValidCol(col) then
      throw new IndexOutOfBoundsException(
        s"(($row,$col) is not in [0,${rows - 1}] x [0,${cols - 1}]")
    data(row)(col)

  def apply(index: (Int, Int)): T =
    if nonValidRow(index._1) || nonValidCol(index._2) then
      throw new IndexOutOfBoundsException(
        s"($index is not in [0,${rows - 1}] x [0,${cols - 1}]")
    data(index._1)(index._2)

  def apply(rs: Seq[Int], cs: Seq[Int]): Matrix[T] =
    require(rs.nonEmpty && cs.nonEmpty, "rs and cs must be nonempty")
    if nonValidRow(rs.min) || nonValidRow(rs.max) then
      throw new IndexOutOfBoundsException(
        s"Not all elements of rs are in [0,${rows - 1}]")
    if nonValidCol(cs.min) || nonValidCol(cs.max) then
      throw new IndexOutOfBoundsException(
        s"Not all elements of cs are in [0,${cols - 1}]")
    val a = Array.tabulate(rs.length, cs.length) {
      (i, j) => data(rs(i))(cs(j))
    }
    new Matrix[T](a)

  def update(row: Int, col: Int, v: T): Unit =
    if nonValidRow(row) || nonValidCol(col) then
      throw new IndexOutOfBoundsException(
        s"(($row,$col) is not in [0,${rows - 1}] x [0,${cols - 1}]")
    data(row)(col) = v

  def update(index: (Int, Int), v: T): Unit =
    if nonValidRow(index._1) || nonValidCol(index._2) then
      throw new IndexOutOfBoundsException(
        s"($index is not in [0,${rows - 1}] x [0,${cols - 1}]")
    data(index._1)(index._2) = v

  // Collection operations

  def exists(p: T => Boolean): Boolean =
    data.exists(_.exists(p))

  def forall(p: T => Boolean): Boolean =
    data.forall(_.forall(p))

  def map[U: Numeric : ClassTag](f: T => U): Matrix[U] =
    val a = data.map(_.map(f))
    new Matrix[U](a)

  // Matrix operation

  def t: Matrix[T] = new Matrix[T](data.transpose)

  def unary_- : Matrix[T] =
    val a = Array.tabulate(rows, cols) {
      (i, j) => num.negate(data(i)(j))
    }
    new Matrix[T](a)

  def +(that: Matrix[T]): Matrix[T] =
    require(rows == that.rows && cols == that.cols,
      "Matrices must have the same number of rows and columns!")
    val a = Array.tabulate(rows, cols) {
      (i, j) => num.plus(data(i)(j), that.data(i)(j))
    }
    new Matrix[T](a)

  def -(that: Matrix[T]): Matrix[T] =
    require(rows == that.rows && cols == that.cols,
      "Matrices must have the same number of rows and columns!")
    val a = Array.tabulate(rows, cols) {
      (i, j) => num.minus(data(i)(j), that.data(i)(j))
    }
    new Matrix[T](a)

  def *(scalar: T): Matrix[T] =
    val a = Array.tabulate(rows, cols) {
      (i, j) => num.times(scalar, data(i)(j))
    }
    new Matrix[T](a)

  def *(that: Matrix[T]): Matrix[T] =
    require(cols == that.rows,
      "The number of columns of this matrix must be the same as the number of rows of matrix that")
    val a = Array.tabulate(rows, that.cols) {
      (i, j) =>
        var x: T = num.zero
        for k <- 0 until cols 
        do x = num.plus(x, num.times(data(i)(k), that.data(k)(j)))
        x
    }
    new Matrix[T](a)

  def ^(l: Long): Matrix[T] =
    if rows != cols then
      throw new UnsupportedOperationException(
        "Only a square matrix is conformable for matrix exponentiation")

    @tailrec
    def binaryPow(n: Long, a: Matrix[T], b: Matrix[T]): Matrix[T] =
      if n == 0 then
        Matrix.identity[T](rows)
      else if n == 1 then
        a * b
      else if n % 2 == 0 then
        binaryPow(n / 2, a * a, b)
      else
        binaryPow(n - 1, a, a * b)

    binaryPow(l, this, Matrix.identity[T](rows))

  override def equals(that: Any): Boolean = that match
    case that: Matrix[T] => rows == that.rows && cols == that.cols && (data.flatten sameElements that.data.flatten)
    case _               => false

  override def hashCode: Int =
    var hash: Int = 1
    for
      i <- 0 until rows
      j <- 0 until cols
    do
      hash = hash * 31 + num.toInt(data(i)(j))
    hash
      
  override def toString: String =
    val maxWidth: Int = data.flatten.map(_.toString.length).max
    val sb = new StringBuilder()
    for
      i <- 0 until rows
      j <- 0 until cols
    do
      sb ++= data(i)(j).toString.reverse.padTo(maxWidth, ' ').reverse
      if j == cols - 1
      then sb += '\n'
      else sb += ' '
    sb.toString
    
  object Matrix:
        
    def apply[T: Numeric : ClassTag](a: Array[T], as: Array[T]*): Matrix[T] =
      new Matrix[T]((a +: as).toArray)

    def tabulate[T: Numeric : ClassTag](rows: Int, cols: Int, f: (Int, Int) => T): Matrix[T] =
      val a = Array.tabulate(rows, cols)(f)
      new Matrix[T](a)

    def fill[T: Numeric : ClassTag](rows: Int, cols: Int, v: => T): Matrix[T] =
      val a = Array.fill(rows, cols)(v)
      new Matrix[T](a)

    def identity[T: ClassTag](n: Int)(using num: Numeric[T]): Matrix[T] =
      val a = Array.tabulate(n, n) {
        (i, j) => if i == j then num.one else num.zero
      }
      new Matrix[T](a)

    def zero[T: ClassTag](n: Int)(using num: Numeric[T]): Matrix[T] =
      val a = Array.fill(n, n)(num.zero)
      new Matrix[T](a)