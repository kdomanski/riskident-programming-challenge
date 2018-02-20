import java.io.{InputStream, PrintStream, BufferedReader, InputStreamReader}
import scala.util.{Try, Success, Failure}

object OrderProcessor {
  case class Customer(arrival: Int, timeToCook: Int)

  /**
    * @param in: An InputStream, which contains the following input:
    * A line containing a single number: The number of guests G,
    * Followed by G lines containing two numbers Oi and Di separated by space.
    * There may be a trailing newline.
    * Oi ist the ordering time for Gi, Di is the time it takes to bake Gi's pizza.
    * 0 <= G <= 100000
    * 0 <= Oi <= 1000000000
    * 1 <= Di <= 1000000000
    *
    * @param out: A PrintStream, which process writes the following output to:
    * A single line containing the integer part of the average waiting time if the input is valid.
    * A single line starting with the words "Syntax error" and an optional description otherwise.
    * There may be a trailing newline.
    */
  def process(in: InputStream, out: PrintStream): Unit = {
    val reader = new BufferedReader(new InputStreamReader(in))
    val customers = Try(parseInput(in))
    if (customers.isFailure) {
      //customers.
      out.print(s"""syntax error: $customers""")
      return
    }

    val lowest = findLowestAverage(customers.get)
    out.print(lowest.floor.toInt)
  }

  def findLowestAverage(customers: Array[Customer]): Float = {
    var incoming = customers.sortBy(_.arrival)
    var waiting = new Array[Customer](0) // customers who have already entered the venue and placed their order
    // NOTE: I could have also used a priority queue.
    var t: Int = 0 // current time
    var waitingTimes = new Array[Float](0) // the individual waiting times

    while (incoming.nonEmpty || waiting.nonEmpty) {
      while (waiting.isEmpty) {
        // see who arrived
        val justArrived = incoming.filter(_.arrival <= t)
        waiting = justArrived.sortBy(_.timeToCook) // pre sort by waiting time
        incoming = incoming.drop(justArrived.length)

        // time fast forward to the next customer
        if (waiting.isEmpty) {
          t = incoming.head.arrival
        }
      }

      // get the client with the shorthest waiting time
      // - which is the first one, becaus we pre-sorted before
      val clientToServe = waiting.head
      // cook his food
      t += clientToServe.timeToCook
      val timeWaited = t - clientToServe.arrival
      waitingTimes = waitingTimes :+ timeWaited.toFloat
      waiting = waiting.drop(1)
    }

    return waitingTimes.sum / waitingTimes.length
  }

  def verifyInputRange(o1: Int, d1: Int): Boolean = {
    (0 <= o1 && o1 <= 1000000000) &&
    (1 <= d1 && d1 <= 1000000000)
  }

  def parseInput(in: InputStream): Array[Customer] = {
    val reader = new BufferedReader(new InputStreamReader(in))
    val G = reader.readLine().toInt

    lazy val lineToCustomer = { l: String =>
      l.split(' ').map(_.toInt) match {
        case Array(oi, di) =>
          if (!verifyInputRange(oi, di)) {
            throw new Exception("value out of range")
          }
          Customer(oi, di)
        case _ => throw new Exception("expected two Integers on the line")
      }
    }

    // read G lines and then try to convert them to Oi, Di pairs
    val lines = for (_ <- 0 until G) yield reader.readLine()
    lines.map(lineToCustomer).toArray
  }
}
