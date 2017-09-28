package scala.macros.tests.scaladays

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SyntaxSuite {
  import TestMacros.syntax
  @Test def char(): Unit = assertEquals("'2'", syntax('2'))
  @Test def double(): Unit = assertEquals("2.0d", syntax(2d))
  @Test def float(): Unit = assertEquals("2.0f", syntax(2f))
  @Test def int(): Unit = assertEquals("2", syntax(2))
  @Test def long(): Unit = assertEquals("2L", syntax(2L))
  @Test def `null`(): Unit = assertEquals("null", syntax(null))
  @Test def string(): Unit = assertEquals("\"2\"", syntax("2"))
  @Test def symbol(): Unit = assertEquals("'S", syntax('S))

  // special cases
  @Test def doubleNaN(): Unit =
    assertEquals("Double.NaN", syntax(Double.NaN))
  @Test def doublePosInf(): Unit =
    assertEquals("Double.PositiveInfinity", syntax(Double.PositiveInfinity))
  @Test def doubleNegInf(): Unit =
    assertEquals("Double.NegativeInfinity", syntax(Double.NegativeInfinity))
  @Test def floatNaN(): Unit =
    assertEquals("Float.NaN", syntax(Float.NaN))
  @Test def floatPosInf(): Unit =
    assertEquals("Float.PositiveInfinity", syntax(Float.PositiveInfinity))
  @Test def floatNegInf(): Unit =
    assertEquals("Float.NegativeInfinity", syntax(Float.NegativeInfinity))

  val x = "a"
  @Test def name(): Unit = assertEquals("SyntaxSuite.this.x", syntax(x))

  @Test def apply0(): Unit =
    assertEquals("\"a\".trim()", syntax("a".trim))
  @Test def apply1(): Unit =
    assertEquals("\"a\".charAt(0)", syntax("a".charAt(0)))
  @Test def apply2(): Unit =
    assertEquals("\"a\".substring(0, 1)", syntax("a".substring(0, 1)))

}
