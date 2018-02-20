import java.io.ByteArrayInputStream
import scala.util.{Try, Success, Failure}

import org.scalatest._

class ParseTests extends FunSuite {
  test("empty input") {
    val testString = ""
    val testInputStream = new ByteArrayInputStream(testString.getBytes)
    val result = Try(OrderProcessor.parseInput(testInputStream))
    assert(result.isFailure)
  }

  test("non numeric first line") {
    val testString = "fsadfas"
    val testInputStream = new ByteArrayInputStream(testString.getBytes)
    val result = Try(OrderProcessor.parseInput(testInputStream))
    assert(result.isFailure)
  }

  test("float first line") {
    val testString = "2.0\n1 2\n3 4"
    val testInputStream = new ByteArrayInputStream(testString.getBytes)
    val result = Try(OrderProcessor.parseInput(testInputStream))
    assert(result.isFailure)
  }

  test("too few inputs") {
    val testString = "2\n1 2"
    val testInputStream = new ByteArrayInputStream(testString.getBytes)
    val result = Try(OrderProcessor.parseInput(testInputStream))
    assert(result.isFailure)
  }

  test("correct input") {
    val testString = "2\n" +
      "1 2\n" +
      "3 4"
    val testInputStream = new ByteArrayInputStream(testString.getBytes)
    val result = Try(OrderProcessor.parseInput(testInputStream))
    assert(result.isSuccess)
    assert(result.get.length == 2)
  }

  test("value out of range") {
    val testString = "2\n" +
      "1 2\n" +
      "1000000001 4"
    val testInputStream = new ByteArrayInputStream(testString.getBytes)

    val result = Try(OrderProcessor.parseInput(testInputStream))
    assert(result.isFailure)
  }
}
