import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}

import scala.util.Try
import org.scalatest._

class ProcessTest extends FunSuite {
  test("syntax error") {
    val byteArray = new ByteArrayOutputStream()
    val ps = new PrintStream(byteArray, true)

    val testString = ""
    val testInputStream = new ByteArrayInputStream(testString.getBytes)

    OrderProcessor.process(testInputStream, ps)

    val expectedString =
      "syntax error: Failure(java.lang.NumberFormatException: null)"
    assert(
      byteArray.toString equals expectedString,
      s"""Expected '$expectedString', but got '${byteArray.toString}' instead."""
    )
  }

  test("test set 1") {
    val byteArray = new ByteArrayOutputStream()
    val ps = new PrintStream(byteArray, true)

    val testString = "3\n0 3\n1 9\n2 6"
    val testInputStream = new ByteArrayInputStream(testString.getBytes)

    OrderProcessor.process(testInputStream, ps)

    val expectedString = "9"
    assert(
      byteArray.toString equals expectedString,
      s"""Expected '$expectedString', but got '${byteArray.toString}' instead."""
    )
  }

  test("test set 2") {
    val byteArray = new ByteArrayOutputStream()
    val ps = new PrintStream(byteArray, true)

    val testString = "3\n0 3\n1 9\n2 5"
    val testInputStream = new ByteArrayInputStream(testString.getBytes)

    OrderProcessor.process(testInputStream, ps)

    val expectedString = "8"
    assert(
      byteArray.toString equals expectedString,
      s"""Expected '$expectedString', but got '${byteArray.toString}' instead."""
    )
  }
}
