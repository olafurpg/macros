package scala.macros.tests.utest

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert
@RunWith(classOf[JUnit4])
class TracerSuite {
  @Test def name(): Unit = {
    val a = true
    val b = false
    val expectedA = TestValue("a", "Boolean", true)
    val expectedB = TestValue("b", "Boolean", false)
    Assert.assertEquals((true, List(expectedA)), Tracer.test(a))
    Assert.assertEquals((true, List(expectedB)), Tracer.test(!b))
//    Assert.assertEquals((true, List(expectedA), expectedB), Tracer.test(a != b))
  }
  // https://github.com/scalatest/scalatest/issues/276
  def woof(f: => Unit) = "woof"
  def meow(x: Int = 0, y: Int = 3) = "meow"
  @Test def bug276(): Unit = {
    val obtained = Tracer.test(woof { meow(y = 5) } == "ugh")
  }
}
