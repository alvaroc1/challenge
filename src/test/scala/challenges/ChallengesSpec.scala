package challenges

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.concurrent.ScalaFutures
import scala.concurrent.duration._
import scala.annotation.tailrec

class ChallengesSpec extends FlatSpec with ScalaFutures with Matchers {
  import PalindromeChallenge.{isPalindromeV2 => isPalindrome}

  "isPalindrome" should "evaluate correctly" in {
    isPalindrome(1)       should be (true)
    isPalindrome(267)     should be (false)
    isPalindrome(9876789) should be (true)
    isPalindrome(-1)      should be (false)
  }

  "Chuck Norris api" should "work" in {
    import ChuckNorrisChallenge._

    whenReady(jokeCategory("science"), timeout(5.seconds)) {joke =>
      joke.nonEmpty should be (true)
    }
    whenReady(jokeCategory("not a category"), timeout(5.seconds)) {joke =>     
      joke.nonEmpty should be (false)
    }
    whenReady(jokeSearch("blue"), timeout(5.seconds)) {joke =>
      joke.nonEmpty should be (true)
    }
    whenReady(jokeSearch("this string is not found in the joke database"), timeout(5.seconds)) {joke =>
      // this seems to always return a joke
      joke.nonEmpty should be (true)
    }
  }

  "StatsChallenge" should "work" in {
    import StatsCalculationChallenge._
    
    calculateStats("1 5 2 4 3") should be (
      Stats(
        mean = 3.0,
        min = 1,
        max = 5,
        median = 3.0,
        sum = 15,
        standardDeviation = 1.4142135623730951
      )
    )

    assertThrows[IllegalArgumentException] {
      calculateStats("")
    }

    assertThrows[IllegalArgumentException] {
      calculateStats("hello world")
    }
  }
}
