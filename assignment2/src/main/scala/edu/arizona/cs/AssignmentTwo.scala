package edu.arizona.cs

import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.control._
//import scala.collection.mutable.LinkedList

//var positionIndex //= new HashMap[String,  List[Int]].withDefaultValue(Nil)

object AssignmentTwo {
  val positionIndex = new HashMap[String,  List[Int]].withDefaultValue(Nil)

    def main(args: Array[String]) {
      /*
      if(args.length==0) {println("No query given"); return;}
      if(args.length < 3 || (args.length%2==0)){
        println("Invalid query. Please try again with a valid query")
        return
      }*/

      val invertedIndex = new HashMap[String,  List[Int]].withDefaultValue(Nil)
      //val positionIndex = new HashMap[String,  List[Int]].withDefaultValue(Nil)

      val filename = "data.txt"
      for (line <- Source.fromFile(filename).getLines) {
          //println(line)
          var words = line.split("\t")
          var i=0
          var docid = ""
          for(parts<- words )
            {
              if(i==0){
              //  println("it is doc id")
                i=1
                docid = parts
              }
              else{
              //  println("tokens")
                i=0
                var position=0;
                for(token <- parts.split(" ")){
                  position = position + 1
                  //println(token)
                  var docIDint = docid.substring(3, docid.length).toInt
                  invertedIndex(token) ::= docIDint
                  positionIndex(token+docIDint) ::= position
                }
              }
            //  println(parts)
            }

      }

/*
      for(v <- invertedIndex.keys){
        //println(v)
        invertedIndex(v) = invertedIndex(v).sorted
        println(v + "   " + invertedIndex(v))
      }*/



      //var result: List[Int] =List()
      var index = 0;
      var k = args(1).substring(1, args(1).length).toInt
      //var result = new HashMap[Int,  List[Int]].withDefaultValue(Nil)

      var result = PositionalIntersect(args(0), invertedIndex(args(0)) , args(2), invertedIndex(args(2)), k )
      /*
      result = invertedIndex(args(0))
      for( index <- 1 until (args.length-1) by 2){
        //println("index= "+index + "args(index)= " +args(index))
        if(args(index).toUpperCase()=="AND") result = Intersect(result,invertedIndex(args(index+1)))
         else if(args(index).toUpperCase()=="OR") result = UNION(result,invertedIndex(args(index+1)))
      //  println(result)

    }*/

/*
      // Print the answer here
      if(result.length==0) println("{}")

      index = 0;
      for( index <- 0 until result.length){
         println( "Doc " + result(index) );
      }*/

    }




    def PositionalIntersect(t1:String, p1:List[Int], t2:String, p2:List[Int], k:Int ) : Int = {
      println(p1)
      println(p2)
      println("k= " + k)
      val answer = new HashMap[Int,  List[Int]].withDefaultValue(Nil)
      /*
      for(v <- positionIndex.keys){
        //println(v)
        positionIndex(v) = positionIndex(v).sorted
        println(v + "   " + positionIndex(v))
      }*/

      var index_p1=0
      var index_p2=0
      var index_pp1=0
      var index_pp2=0
      val listTemp = new ListBuffer[Int]()
      val pp1:List[Int] = positionIndex( t1 + p1(index_p1) )
      val pp2:List[Int] = positionIndex( t2 + p2(index_p2) )

      val loopbreak = new Breaks

      while (index_p1 < p1.length && index_p2 < p2.length){
        if (p1(index_p1) == p2(index_p2)){
          listTemp.clear

          index_pp1 = 0
          index_pp2 = 0
          while(index_pp1 < pp1.length){
            loopbreak.breakable{
            while(index_pp2 < pp2.length){
              if( Math.abs(pp1(index_pp1) -  pp2(index_pp2))<= k )
                listTemp += pp2(index_pp2)
              else if(pp2(index_pp2) >  pp1(index_pp1)) loopbreak.break;
              index_pp2 = index_pp2+1
            }}

            while(!listTemp.isEmpty && Math.abs(listTemp(0) - pp1(index_pp1)) > k){
                listTemp.remove(0)
                for(ps<-listTemp){
                    answer(p1(index_p1)) ::= ps
                }
            index_p1 = index_p1 +1
            }
          }
          index_p1 = index_p1 +1
          index_p2 = index_p2 +1
        }
        else if (p1(index_p1) < p2(index_p2))
          index_p1 = index_p1+1
        else index_p2 = index_p2+1
      }
      println(answer)
return 0
}

}
