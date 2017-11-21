package scala.macros.tests

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class FormatInterpolatorSuite {
  @Test def example(): Unit = {
    FormatInterpolator.printf("I have %d bananas and %d mangos", 20, 5)
  }
}
