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
		println("===" + pairNumber + "==================new data in new line !!!")
      		//println(line)
		val jsonResult = JSON.parseFull(line)
		val map:Map[String,Any] = jsonResult.get.asInstanceOf[Map[String, Any]]


		val goldLabel = map.get("gold_label").get.asInstanceOf[String]
		val sentence1 = map.get("sentence1").get.asInstanceOf[String]
		val sentence2 = map.get("sentence2").get.asInstanceOf[String]
		val sentence1_parse = map.get("sentence1_parse").get.asInstanceOf[String]
		//val sentence1_parse:Map[String,Any] = map.get("sentence1_parse").get.asInstanceOf[Map[String, Any]]	
		//val sentence1_parse_extract_VBG = sentence1_parse.get("VBG").get.asInstanceOf[String]	

println("goldLabel=" + goldLabel )
println("sentence1=" + sentence1 )
//loadInvertedIndex(sentence1)
println("sentence2=" + sentence2 )

println("sentence1_parse=" + sentence1_parse )
//for(VBG<-sentence1_parse_extract_VBG){
//println("extracted=" + VBG )}


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

    def loadInvertedIndex(premise:String){
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

	for(v <- invertedIndex.keys){
		println(v + "   " + invertedIndex(v))
	      }

    }

}
