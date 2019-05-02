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
import util.control.Breaks._


 
import LanguageInference._


object LanguageInference {
    
    var parameterSimilarity = 0.5
    var parameterSimilarityDifference = 0.35
    val ENTAILMENT = 2
    val NEUTRAL = 1
    val CONTRADICTION = 0
    
    
    var cut1 = 0.18
    var cut2 = 0.36
    
    //for evaluating
    var truePositives:Array[Int] = new Array[Int](3)
    var falsePositives:Array[Int] = new Array[Int](3)
    var trueNegatives:Array[Int] = new Array[Int](3)
    var falseNegatives:Array[Int] = new Array[Int](3)

    //for training
    var maxScores:Array[Double] = new Array[Double](3)
    var minScore:Array[Double] = new Array[Double](3)
    var averageScore:Array[Double] = new Array[Double](3)
    var aveVerbTokenSimilarityDiff:Array[Double] = new Array[Double](3)

    
//  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

    val invertedIndex = new HashMap[String,  Int]
    val proc:Processor = new CoreNLPProcessor() 
    var doc = proc.annotate("")  
    
//////////////////////////////////////////////////////////
    val jsonFileName = "snli_1.0_train_small.jsonl"
    var fileNameToOperate = "train_fiveH.jsonl"
	var premiseList = new ArrayBuffer[String]()   //:List[String] = List()    
	var hypothesisList = new ArrayBuffer[String]() //:List[String] = List()    
	var premiseVerbsList = new ArrayBuffer[String]()    //:List[String] = List()    
	var hypothesisVerbsList  = new ArrayBuffer[String]()  // :List[String] = List()    
	var actualClassList  = new ArrayBuffer[Int]() // :List[String] = List()    
	


    def main(args: Array[String]) {
      train("snli_1.0_dev.jsonl")//"snli_1.0_train.jsonl")//jsonFileName)
       
      operationOnFile("snli_1.0_test.jsonl")
     
    // var inferredClass= classify("Two women are embracing while holding to go packages.","The sisters are hugging goodbye while holding to go packages after just eating lunch.")  //"The men are fighting outside a deli.")
    // println("inferredClass = "+inferredClass)
    }
    
    
    
    def train(fileName:String){
        var totalScore:Array[Double] = new Array[Double](3)
        var totalVerbTokenSimilarityDif:Array[Double] = new Array[Double](3)
        
        for (i <- 0 to 2) {
            maxScores(i) = 0.0
            minScore(i) = 10.0
            totalScore(i) = 0.0
            totalVerbTokenSimilarityDif(i) = 0.0
        }
        
        readJsonDataFile(fileName)
        
        //readLemmatizedData()
        
        var scoreVerbs = 0.0
        var scoreTokens = 0.0
        var score =  0.0
        var pairClass = 0
        var scoreDiff = 0.0

    
        var inferredClass = 0
        var totalPairs = premiseList.length
        for(pairNumber<-0 to totalPairs-1){
            if(pairNumber%200==0) println(" operating on pair : " + pairNumber) 
            breakable{
            //scoreVerbs = JaccardCoefficient(premiseVerbsList(pairNumber), hypothesisVerbsList(pairNumber))
            scoreTokens = JaccardCoefficient(premiseList(pairNumber), hypothesisList(pairNumber))
            score =   scoreTokens //+ scoreVerbs 
            
            pairClass = actualClassList(pairNumber)
            if(pairClass==10) break
            totalScore(pairClass) = totalScore(pairClass) + score
            scoreDiff = scoreVerbs - scoreTokens
            totalVerbTokenSimilarityDif(pairClass) = totalVerbTokenSimilarityDif(pairClass) + scoreDiff
            if(score> maxScores(pairClass)) maxScores(pairClass) = score
            if(scoreDiff < minScore(pairClass)) minScore(pairClass) = scoreDiff
            
            if(pairNumber %1000==0) println(pairNumber + " pairs processed")
            }//end of break
        }
			

        for (i <- 0 to 2) {
            averageScore(i) = totalScore(i) / totalPairs
            aveVerbTokenSimilarityDiff(i) = totalVerbTokenSimilarityDif(i) / totalPairs
        }
    	
    	
    	// print the values
    	
    	for (i <- 0 to 2) {
    	    println("************** For class " + i)
    	    println("maxScores : " + maxScores(i))
    	    println("minScore : " + minScore(i))
    	    println("averageScore : " + averageScore(i))
    	    println("aveVerbTokenSimilarityDiff : " + aveVerbTokenSimilarityDiff(i))
        }
        
        
        var maximumScore = 0.0
    	for (i <- 0 to 2) {
    	    if(maximumScore <  maxScores(i) ) maximumScore =  maxScores(i) 
    	}
    	
    	cut1 = maximumScore / 3
    	cut2 = 2 * cut1
        
    }

    def readLemmatizedData(){
        premiseList.clear()
        hypothesisList.clear()
        premiseVerbsList.clear()
        hypothesisVerbsList.clear()
            
        val source = Source.fromResource("train_full.txt")
	    var pairNumber = 0

        var premise = ""
	    var hypothesis = ""
	    var premiseVerbs = ""
	    var hypothesisVerbs = ""
	    var verbs = ""
	    var actualClass = 0
	    var temp = ""
	    
	    
	    var lineIndex = 0
        for(line <- source.getLines()) { pairNumber = pairNumber + 1  
            if(lineIndex==2) { 
                lineIndex=0 
                temp = line.trim 
                actualClass = temp match {
                case "contradiction" => CONTRADICTION
                case "neutral" => NEUTRAL 
                case "entailment" =>  ENTAILMENT 
                case "-" =>  10

		    } 
                actualClassList += actualClass
            }
            else if (lineIndex==1) { 
                lineIndex = lineIndex + 1
                hypothesisList += line

            }
            else if (lineIndex==0) { 
                lineIndex = lineIndex + 1
                premiseList += line
            }		                

		}
    }
    
    
    
    def operationOnFile(fileNameToOperate:String){
        var correctClassPairs = 0
        var incorrectClassPairs = 0
        var actualClass = 0
        
        for (i <- 0 to 2) {
            truePositives(i) = 0
            falsePositives(i) = 0
            trueNegatives(i) = 0
            falseNegatives(i) = 0
        }
        
        readJsonDataFile(fileNameToOperate)
    
        var inferredClass = 0
        for(pairNumber<-0 to premiseList.length-1){
           // inferredClass = classify(premiseList(pairNumber), hypothesisList(pairNumber))//, premiseVerbsList(pairNumber), hypothesisVerbsList(pairNumber)) 
            inferredClass = classifyConsideringVerbs(premiseList(pairNumber), hypothesisList(pairNumber), premiseVerbsList(pairNumber), hypothesisVerbsList(pairNumber)) 
            
            //println(inferredClass)
            
            actualClass = actualClassList(pairNumber)
            if(actualClass == inferredClass)  correctClassPairs = correctClassPairs + 1
                else incorrectClassPairs = incorrectClassPairs + 1
     
            for (i <- 0 to 2) {
                if( i== actualClass) {
                    if(i == inferredClass)
                        truePositives(i) = truePositives(i) + 1
                    else{
                        falseNegatives(i) = falseNegatives(i) + 1
                    }
                }
                else{
                     if(i == inferredClass)
                        trueNegatives(i) = trueNegatives(i) + 1
                    else{
                        falsePositives(i) = falsePositives(i) + 1
                    }                   
                }
                
            }        
        }
        
        
        
        println("correctClassPairs = " + correctClassPairs)
        println("incorrectClassPairs = " + incorrectClassPairs)
        println("Total Pairs = " + (correctClassPairs + incorrectClassPairs) )

    	
        var TP = 0
        var FP = 0
        var TN = 0
        var FN = 0  
        for (i <- 0 to 2) {
            TP = TP + truePositives(i)
            FP = FP + falsePositives(i)
            TN = TN + trueNegatives(i)
            FN = FN + falseNegatives(i) 
        
            println("F1 score for class " + i + " : ")            
            calculateF1(TP, FP, TN, FN)        
        }

        println("F1 score for total system : ")            
        calculateF1(TP, FP, TN, FN)
        
    }

    
    def calculateF1(TP:Int, FP:Int, TN:Int, FN:Int) {
        def precision:Double = TP.toDouble / (TP + FP)
        def recall:Double = TP.toDouble / (TP + FN)
        def f1:Double = 2.0 * precision * recall / (precision + recall)
    
        println(s"P = $precision, R = $recall, F1 = $f1")
    }

    
    
    def classify(premise:String, hypothesis:String) : Int = {  //, premiseVerbs:String, hypothesisVerbs:String) : Int = {
           // val scoreVerbs = JaccardCoefficient(premiseVerbs, hypothesisVerbs)
            val scoreTokens = JaccardCoefficient(premise, hypothesis)
            val score =   scoreTokens //+ scoreVerbs
            //println("JaccardCoefficient = " +  score  + "     scoreVerbs="  + scoreVerbs   + "    scoreTokens=" + scoreTokens)    
            
            var inferredClass = NEUTRAL
            
            // scored only on tokens
            if(score < cut1) inferredClass = CONTRADICTION
                else {
                    if(score < cut2 ) inferredClass = NEUTRAL 
                        else inferredClass = ENTAILMENT
                }
                
            
            
            // scored  on tokens and verbs
            /*if(score< parameterSimilarity) inferredClass = CONTRADICTION
                else {
                    //if(scoreVerbs - scoreTokens> parameterSimilarityDifference  ) inferredClass = ENTAILMENT
                       // else inferredClass = NEUTRAL
                }
                
              */  
                
                
            return inferredClass
    }
    

    def classifyConsideringVerbs(premise:String, hypothesis:String, premiseVerbs:String, hypothesisVerbs:String) : Int = {
            val scoreVerbs = JaccardCoefficient(premiseVerbs, hypothesisVerbs)
            val scoreTokens = JaccardCoefficient(premise, hypothesis)
            val score =   scoreTokens + scoreVerbs
            //println("JaccardCoefficient = " +  score  + "     scoreVerbs="  + scoreVerbs   + "    scoreTokens=" + scoreTokens)    
            
            var inferredClass = NEUTRAL
                
            
            
            // scored  on tokens and verbs
            if(score< parameterSimilarity) inferredClass = CONTRADICTION
                else {
                    if(scoreVerbs - scoreTokens> parameterSimilarityDifference  ) inferredClass = ENTAILMENT
                        else inferredClass = NEUTRAL
                }
                
                
            return inferredClass
    }


    def readJsonDataFile(fileNameToOperate:String){
        premiseList.clear()
        hypothesisList.clear()
        premiseVerbsList.clear()
        hypothesisVerbsList.clear()
            
        val source = Source.fromResource(fileNameToOperate)
	    var pairNumber = 0

        var premise = ""
	    var hypothesis = ""
	    var premiseVerbs = ""
	    var hypothesisVerbs = ""
	    var verbs = ""
	    
	    var buf = new StringBuilder
	    
	    
        for(line <- source.getLines()) { pairNumber = pairNumber + 1
		    //println("=== pairNumber : " + pairNumber + "==================")
		    
		    // parse data from json 
		    val jsonResult = JSON.parseFull(line)
		    val map:Map[String,Any] = jsonResult.get.asInstanceOf[Map[String, Any]]
		    val goldLabel = map.get("gold_label").get.asInstanceOf[String]
		    val sentence1 = map.get("sentence1").get.asInstanceOf[String]
		    val sentence2 = map.get("sentence2").get.asInstanceOf[String]
		    
		  breakable{
		      if(goldLabel=="-") break
		      if(sentence1.length<3) break
		      if(sentence2.length<3) break
		      
		   
		    // convert goldLable string to integer
		    val actualClass = goldLabel match {
                case "contradiction" => CONTRADICTION
                case "neutral" => NEUTRAL 
                case "entailment" =>  ENTAILMENT 
		    }   
		    
		    //print(actualClass)

	        premise = sentence1
	        hypothesis = sentence2

            //lemmatize
	        premise = ""
	        hypothesis = ""
	        
	        premise = sentence1
		    hypothesis = sentence2
		    
            /*
            buf.setLength(0)
            doc.clear
            doc = proc.annotate(sentence1)
            for (sentence <- doc.sentences) {
                sentence.lemmas.foreach(lemmas => buf ++=  lemmas.mkString(" ") )  //premise = premise + lemmas.mkString(" ") ) //println(s"Lemmas: ${lemmas.mkString(" ")}"))
            }
            premise = buf.toString
           


            //val bufHypothesis = new StringBuilder
            buf.setLength(0)
            doc.clear
            doc = proc.annotate(sentence2)
            for (sentence <- doc.sentences) {
                sentence.lemmas.foreach(lemmas =>  buf ++=  lemmas.mkString(" ") ) //hypothesis = hypothesis + lemmas.mkString(" ") ) //println(s"Lemmas: ${lemmas.mkString(" ")}"))
            }
            hypothesis = buf.toString
 */

		    //trimming the period from the end
		    premise = premise.replace(".","")
		    hypothesis = hypothesis.replace(".","")
		   


	    	
		    val verbPattern = new Regex("\\s.VB.\\s(\\w*)")
		    val sentence1_parse = map.get("sentence1_parse").get.asInstanceOf[String]
		
	    	verbs = "dummy "
		    for (patternMatch <- verbPattern.findAllIn(sentence1_parse))
			    verbs = verbs + " " + patternMatch.substring(6, patternMatch.length)
			
            premiseVerbs = verbs
            /*
            doc.clear
            doc = proc.annotate(verbs)
            for (sentence <- doc.sentences) {
                sentence.lemmas.foreach(lemmas => premiseVerbs = premiseVerbs + lemmas.mkString(" ") )
                }
            premiseVerbs = premiseVerbs.substring(5, premiseVerbs.length)
            //println(premiseVerbs)
            */

            verbs = "dummy "
		    val sentence2_parse = map.get("sentence2_parse").get.asInstanceOf[String]
		    for (patternMatch <- verbPattern.findAllIn(sentence2_parse))
		    	verbs = verbs + " " + patternMatch.substring(6, patternMatch.length)
			
            hypothesisVerbs = verbs
            /*
            doc.clear
            doc = proc.annotate(verbs)
            for (sentence <- doc.sentences) {
                sentence.lemmas.foreach(lemmas => hypothesisVerbs = hypothesisVerbs + lemmas.mkString(" ") )
            }
            hypothesisVerbs = hypothesisVerbs.substring(5, hypothesisVerbs.length)
            */
            //println(hypothesisVerbs)

            
            
		    actualClassList += actualClass
            premiseList += premise
            hypothesisList += hypothesis
            premiseVerbsList += premiseVerbs
            hypothesisVerbsList += hypothesisVerbs

          }//end of breakable    
          
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
