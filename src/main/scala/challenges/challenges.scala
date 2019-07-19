package challenges

// PALINDROME

/**
 * There's probably a more efficient algorithm that uses ints directly with some
 * bit-shifting or modulus, or something like that
 */
object PalindromeChallenge {
  import scala.annotation.tailrec

  /** 
   * simple version, but traverses the string more than necessary,
   * ok in most cases since ints don't produce large strings
   */
  def isPalindromeV1 (n: Int): Boolean = n.toString == n.toString.reverse

  /**
   * might be more efficient since it short-circuits
   * but something other than String would be better, 
   * since repeatedly calling .init will end up traversing a lot
   * to do better, I would have to do some research and benchmark
   */
  def isPalindromeV2 (n: Int): Boolean = {
    @tailrec def isPalindromeString (s: String): Boolean = s match {
      case x if x.size <= 1 => true // one or zero chars is always true
      case x if x.head == x.last => isPalindromeString(x.tail.init) // drop the first and last chars 
      case _ => false
    }
    isPalindromeString(n.toString)
  }

}

// Chuck Norris
import play.api.libs.ws.StandaloneWSClient
import play.api.libs.ws.JsonBodyReadables._
import play.api.libs.ws.ahc.StandaloneAhcWSClient
import play.api.libs.json.JsValue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ChuckNorrisChallenge {
  // http client
  private val ws: StandaloneWSClient = {
    import akka.actor.ActorSystem
    import akka.stream.ActorMaterializer
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    StandaloneAhcWSClient()
  }

  /** @param param Param name and value, ex: "query" -> "some query" */
  private def makeJokeRequest (param: (String,String)): Future[Option[String]] = 
    ws.url(s"https://api.chucknorris.io/jokes/random")
      .withQueryStringParameters(param)
      .execute()
      .map {res => 
        res.status match {
          case 200 => Some((res.body[JsValue] \ "value").as[String])
          case 404 => None
          // keeping the api simple for now,
          // just throw on anything else
          case code => sys.error(s"Unsupported response code $code when requesting joke for $param")
        }
      }

  def jokeCategory (category: String): Future[Option[String]] = makeJokeRequest("category" -> category)

  def jokeSearch (query: String): Future[Option[String]] = makeJokeRequest("query" -> query)

}

// STATS
object StatsCalculationChallenge {

  /** 
   * throws on invalid string
   * @param numbers Space delimited list of integers 
   */
  def calculateStats (numbers: String): Stats = {
    // parse and validate
    val arr = numbers.trim.split(" ").map {num =>
      try (num.toInt) catch {
        case ex: NumberFormatException => throw new IllegalArgumentException(s"Input must be all integers, got [$num] in [$numbers]")
      }
    }.sorted.toList
    require(arr.nonEmpty, s"There must be at least 1 number, got: $numbers")

    def minOf (a: Int, b: Int) = if (a < b) a else b
    def maxOf (a: Int, b: Int) = if (a > b) a else b

    val first = arr.head

    // we should be able to produce mean, min, max, sum in one go
    val (size, mean, min, max, sum) = 
      ((1, first.toDouble, first, first, first) /: arr.tail) {case ((count, accMean, accMin, accMax, accSum), v) =>
        (
          count + 1,
          ((accMean * count) + v) / (count + 1), // recursive mean
          minOf(accMin, v),
          maxOf(accMax, v),
          accSum + v
        )
      }

    val median: Double = 
      if (size % 2 == 0) {
        // index of the first middle number
        val idx = size / 2 - 1
        (arr(idx) + arr(idx+1)) / 2d // average of both middle numbers
      } else arr(size / 2)

    // i think this could be calculated in one go recursively with the rest,
    // but it might actually be more computation, would have to benchmark to be sure
    // also not sure what should happen in case of a single value... would have to research a bit to be sure
    val standardDeviation: Double = math.sqrt(
      arr.map {v =>
        math.pow(v - mean, 2)
      }.foldLeft(0d)(_ + _) / size
    )

    Stats(
      mean = mean,
      min = min,
      max = max,
      median = median,
      sum = sum,
      standardDeviation = standardDeviation
    )
  }
}

case class Stats(
  mean: Double,
  min: Int,
  max: Int,
  median: Double,
  sum: Int,
  standardDeviation: Double
)

/* SCASTIE
import PalindromeChallenge.{isPalindromeV2 => isPalindrome}
isPalindrome(1) 
isPalindrome(267)  
isPalindrome(9876789) 
isPalindrome(-1)  

import ChuckNorrisChallenge._
// just for easy demo, not for actual use
def await [T](f: Future[T]): T = {
  import scala.concurrent.Await
  import scala.concurrent.duration._
  Await.result(f, 5.seconds)
}

await(jokeCategory("science"))
await(jokeCategory("not a category"))
await(jokeSearch("blue"))
// this one seems to always return a joke, regardless of the query :/
await(jokeSearch("this string is not found in the joke database"))

import StatsCalculationChallenge._
calculateStats("1 5 2 4 3")
*/