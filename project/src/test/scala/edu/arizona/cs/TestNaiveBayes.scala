package edu.arizona.cs

import org.scalatest._

class TestNaiveBayes extends FlatSpec with Matchers {
 
  val nb = new NaiveBayes
  nb.trainMultinomial("spamDataset")

  "naive bayes" should "perform well on spamDataset" in {
    val score = nb.evaluate("spamDataset")

    println(s"Classification score for the SPAM category: $score")

    (score.precision > 0.95) should be (true)
    (score.recall > 0.95) should be (true)
    (score.f1 > 0.95) should be (true)
  }
}
