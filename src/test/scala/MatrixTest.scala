import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

class MatrixTest extends TestCase:

  val m1 = Matrix(Array(1, 2, 3), Array(4, 5, 6))
  val m2 = Matrix(Array(2, 3, 1), Array(0, 5, 6), Array(1, 1, 2))

  def testRow: Unit =
    assert(m1.rows == 2)
    assert(m2.rows == 3)
  
  def testCols: Unit =
    assert(m1.cols == 3)
    assert(m2.cols == 3)
  
  def testSize: Unit =
    assert(m1.size == (2, 3))
    assert(m2.size == (3, 3))
  
  def testIsSquare: Unit =
    assert(m1.isSquare == false)
    assert(m2.isSquare == true)

  def testApply: Unit =
    assert(m1(1, 2) == 6)
    assert(m1((0, 0)) == 1)

    assert(m2(0, 2) == 1)
    assert(m2((2, 2)) == 2)

    val exp1 = Matrix(Array(1, 3), Array(4, 6))
    assert(m1(0 to 1, 0 to 2 by 2) == exp1)

    val exp2 = Matrix(Array(0, 5, 6))
    assert(m2(1 to 1, 0 to 2) == exp2)

  def testUpdate: Unit =
    val m3 = Matrix(Array(1, 2))
    m3(0, 0) = 2
    assert(m3(0, 0) == 2)

    m3((0, 0)) = 3
    assert(m3(0, 0) == 3)

  def testExists: Unit =
    val p: Boolean = m1.exists(_ == 4)
    assertTrue(p)

  def testForall: Unit =
    val p: Boolean = m2.forall(_ < 5)
    assert(p == false)

  def testMap: Unit =
    val exp = Matrix(Array(6, 7, 8), Array(9, 10, 11))
    assert(m1.map(_ + 5) == exp)

  def testTranspose: Unit =
    def exp = Matrix(Array(1, 4), Array(2, 5), Array(3, 6))
    assert(m1.t == exp)

  def testUnaryMinus: Unit =
    val exp = Matrix(Array(-1, -2, -3), Array(-4, -5, -6))
    assert(-m1 equals exp)

  def testPlus: Unit =
    val m3 = Matrix(Array(2, 2, 2), Array(2, 2, 2))
    val exp = Matrix(Array(3, 4, 5), Array(6, 7, 8))
    assert(m1 + m3 == exp)

  def testMinus: Unit =
    val m3 = Matrix(Array(2, 2, 2), Array(2, 2, 2))
    val exp = Matrix(Array(-1, 0, 1), Array(2, 3, 4))
    assert(m1 - m3 == exp)

  def testScalarMult: Unit =
    val exp = Matrix(Array(3, 6, 9), Array(12, 15, 18))
    assert(m1 * 3 == exp)

  def testMatrixMult: Unit =
    val m3 = Matrix(Array(7, 8), Array(9, 10), Array(11, 12))
    val exp = Matrix(Array(58, 64), Array(139, 154))
    assert(m1 * m3 == exp)

  def testPower: Unit =
    val exp = Matrix(Array(5, 22, 22), Array(6, 31, 42), Array(4, 10, 11))
    assert((m2 ^ 2) == exp)

  def testFill: Unit =
    val exp = Matrix(Array(4, 4, 4, 4), Array(4, 4, 4, 4), Array(4, 4, 4, 4), Array(4, 4, 4, 4))
    assert(Matrix.fill(4, 4, 4) == exp)

  def testIdentity: Unit =
    val exp = Matrix(Array(1, 0, 0), Array(0, 1, 0), Array(0, 0, 1))
    assert(Matrix.identity[Int](3) == exp)

  def testZero: Unit =
    val exp = Matrix(Array(0, 0, 0), Array(0, 0, 0), Array(0, 0, 0))
    assert(Matrix.zero[Int](3) == exp)