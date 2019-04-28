package edu.arizona.cs

import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.processors.{Document, Processor}
import org.clulab.struct.DirectedGraphEdgeIterator


//import edu.stanford.nlp.simple._
//import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import scala.util.parsing.json._
import scala.util.matching.Regex



//import LanguageModel._


object LanguageInference {
//  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

   // val invertedIndex = new HashMap[String,  List[Int]].withDefaultValue(Nil)
    val invertedIndex = new HashMap[String,  Int]
    val totalListBuffer = new ListBuffer[Int]()
    var TotalDocuments = 0
    var TotalTokenCollection = 0
//////////////////////////////////////////////////////////
    val jsonFileName = "snli_1.0_train_small.jsonl"


    def main(args: Array[String]) {
	println("success!!!")
    	val source = Source.fromResource(jsonFileName)
	var pairNumber = 0

    	for(line <- source.getLines()) { pairNumber = pairNumber + 1
		println("=== pairNumber : " + pairNumber + "==================")
      		//println(line)
		val jsonResult = JSON.parseFull(line)
		val map:Map[String,Any] = jsonResult.get.asInstanceOf[Map[String, Any]]


		val goldLabel = map.get("gold_label").get.asInstanceOf[String]
		val sentence1 = map.get("sentence1").get.asInstanceOf[String]
		val sentence2 = map.get("sentence2").get.asInstanceOf[String]

		//trimming the period from the end
		var premise = sentence1.replace(".","")//.substring(0, sentence1.length-1)
		var hypothesis = sentence2.replace(".","")//.substring(0, sentence2.length-1)
		premise = premise.toLowerCase()
		hypothesis = hypothesis.toLowerCase()

//if(goldLabel=="neutral"){
//if(goldLabel=="contradiction"){
//if(goldLabel=="entailment"){
		println("verbs in premise")
		val sentence1_parse = map.get("sentence1_parse").get.asInstanceOf[String]
		val verbPattern = new Regex("\\s.VB.\\s(\\w*)")//("VP\\s.VB.\\s(\\w*)")
		for (patternMatch <- verbPattern.findAllIn(sentence1_parse))
			println("VB. :" + patternMatch.substring(6, patternMatch.length))

		println("verbs in hypothesis")
		val sentence2_parse = map.get("sentence2_parse").get.asInstanceOf[String]
		for (patternMatch <- verbPattern.findAllIn(sentence2_parse))
			println("VB. :" + patternMatch.substring(6, patternMatch.length))
			

println("goldLabel=" + goldLabel )
println("JaccardCoefficient = " + JaccardCoefficient(premise, hypothesis)  )
println("Premise=" + premise )
println("hypothesis=" + hypothesis )
//}

/*
CosineScore(q)
1 float Scores[N] = 0
2 float Length[N]
3 for each query term t
4 do calculate w t,q and fetch postings list for t
5
for each pair(d, tf t,d ) in postings list
6
do Scores[d]+ = w t,d × w t,q
7 Read the array Length
8 for each d
9 do Scores[d] = Scores[d]/Length[d]
10 return Top K components of Scores[]
*/

    	}
    }


    def JaccardCoefficient(premise:String, hypothesis:String) : Double = {
    	loadInvertedIndex(premise)
	var intersectionValue = 0
	var tokensINhypothesis = 0
	for( token <- hypothesis.split(" ") ){ tokensINhypothesis = tokensINhypothesis + 1
		if(invertedIndex.contains(token))
			intersectionValue = intersectionValue + 1
		}

	return (intersectionValue.toDouble)/(invertedIndex.size.toDouble + tokensINhypothesis.toDouble)
	}
   

    def loadInvertedIndex(premise:String){
	invertedIndex.clear
	var frequency = 0
	for( token <- premise.split(" ") ){
		if(!invertedIndex.contains(token)) // If this term is not already in the vocabulary
			{			
                  	invertedIndex+= (token -> 0)
			}
		frequency = invertedIndex(token) +1
		invertedIndex-= (token)
		invertedIndex+= (token -> frequency)
		}
	/*
	for(v <- invertedIndex.keys){
		println(v + "   " + invertedIndex(v))
	      }*/

    }

}
