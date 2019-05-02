package edu.arizona.cs

/**
  * Counts to compute P/R/F1
  */
case class Score (truePositives:Int,
                  falsePositives:Int,
                  trueNegatives:Int,
                  falseNegatives:Int) {
  def precision:Double = truePositives.toDouble / (truePositives + falsePositives)
  def recall:Double = truePositives.toDouble / (truePositives + falseNegatives)
  def f1:Double = 2.0 * precision * recall / (precision + recall)

  override def toString: String = s"P = $precision, R = $recall, F1 = $f1"
}
