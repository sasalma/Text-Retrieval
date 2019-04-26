package edu.arizona.cs

import org.scalatest._

class TestLanguageModel extends FlatSpec with Matchers {
	val lm = new LanguageModel
	lm.train("input.txt")

	"language model" should "rank document correctly with unsmoothed method" in {
		val docs = lm.rank(List("information", "retrieval"), withSmoothing = false).toArray
		println(s"Ranked documents using the unsmoothed method: ${docs.mkString(", ")}")

		docs.size should be (2)
		docs(0).id should be ("Doc1")
		docs(1).id should be ("Doc2")
	}

	it should "rank documents correctly with smoothed method" in {
		val docs = lm.rank(List("information", "retrieval"), withSmoothing = true).toArray
		println(s"Ranked documents using the smoothed method: ${docs.mkString(", ")}")

		docs.size should be (4)
		docs(0).id should be ("Doc1")
		docs(1).id should be ("Doc2")
		docs(2).id should be ("Doc4")
		docs(3).id should be ("Doc3")
	}
}
