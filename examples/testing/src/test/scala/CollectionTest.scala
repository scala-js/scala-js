import org.junit.Test
import org.junit.Assert._

class CollectionTest {
  @Test
  def array_map_and_filter_ints(): Unit = {
    val array = Array(5, 7, 2, 6, -30, 33, 66, 76, 75, 0)
    val result = array.filter(_.toInt % 3 != 0).map(x => x*x)
    assertArrayEquals(Array(25, 49, 4, 76*76), result)
  }
}
