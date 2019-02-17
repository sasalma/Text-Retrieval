package edu.arizona.cs

import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.control._

object AssignmentTwo {
  val positionIndex = new HashMap[String,  List[Int]].withDefaultValue(Nil)

    def main(args: Array[String]) {

      if(args.length==0) {println("No query given"); return;}
      if(args.length < 3){
        println("Invalid query. Please try again with a valid query")
        return
      }

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


      for(v <- invertedIndex.keys){
        //println(v)
        invertedIndex(v) = invertedIndex(v).sorted
        //println(v + "   " + invertedIndex(v))
      }
      for(v <- positionIndex.keys){
        //println(v)
        positionIndex(v) = positionIndex(v).sorted
        //println(v + "   " + positionIndex(v))
      }

      var k = args(1).substring(1, args(1).length).toInt
      var direction = 2
      if(args.length>3 && args(3)=="1") direction=1
      var result = PositionalIntersect(args(0), invertedIndex(args(0)) , args(2), invertedIndex(args(2)), k, direction )
    }




    def PositionalIntersect(t1:String, p1:List[Int], t2:String, p2:List[Int], k:Int, direction:Int ) : Int = {
      //println(p1)
      //println(p2)
      //println("k= " + k)
      println("direction = "+ direction)
      val answer = new HashMap[Int,  List[Int]].withDefaultValue(Nil)
      var index_p1=0
      var index_p2=0
      var index_pp1=0
      var index_pp2=0
      val listTemp = new ListBuffer[Int]()
      val loopbreak = new Breaks

      while (index_p1 < p1.length && index_p2 < p2.length){
        if (p1(index_p1) == p2(index_p2)){
          //println("pp1= "+positionIndex( t1 + p1(index_p1) ))
          //println("pp2= "+positionIndex( t2 + p2(index_p2) ))

          listTemp.clear

          index_pp1 = 0
          index_pp2 = 0
          while(index_pp1 < positionIndex( t1 + p1(index_p1) ).length){
            loopbreak.breakable{
            while(index_pp2 < positionIndex( t2 + p2(index_p2) ).length){
              direction match{
                case 1 =>{
                  if((positionIndex( t2 + p2(index_p2) )(index_pp2) - positionIndex( t1 + p1(index_p1) )(index_pp1)>0) && (positionIndex( t2 + p2(index_p2) )(index_pp2) - positionIndex( t1 + p1(index_p1) )(index_pp1)<= k) ){
                    listTemp += positionIndex( t2 + p2(index_p2) )(index_pp2)
                  }
                    else if(positionIndex( t2 + p2(index_p2) )(index_pp2) >  positionIndex( t1 + p1(index_p1) )(index_pp1)) loopbreak.break;
                }

              case 2 =>{
                if( Math.abs(positionIndex( t1 + p1(index_p1) )(index_pp1) -  positionIndex( t2 + p2(index_p2) )(index_pp2))<= k ){
                  listTemp += positionIndex( t2 + p2(index_p2) )(index_pp2)
                }
                  else if(positionIndex( t2 + p2(index_p2) )(index_pp2) >  positionIndex( t1 + p1(index_p1) )(index_pp1)) loopbreak.break;
              }
            }
              index_pp2 = index_pp2+1
            }
            }//end of breakable loop

            while(!listTemp.isEmpty && Math.abs(listTemp(0) - positionIndex( t1 + p1(index_p1) )(index_pp1)) > k){
                listTemp.remove(0)
              }

            for(ps<-listTemp){
                    answer(p1(index_p1)) ::= positionIndex( t1 + p1(index_p1) )(index_pp1)
                    answer(p1(index_p1)) ::= ps
                }
            index_pp1 = index_pp1 +1
          }

          index_p1 = index_p1 +1
          index_p2 = index_p2 +1
        }
        else if (p1(index_p1) < p2(index_p2))
          index_p1 = index_p1+1
        else index_p2 = index_p2+1
      }

      // sort the positional indexes and print the output
      for(v <- answer.keys){
        //println("Doc " + v)
        answer(v) = answer(v).reverse
        println("Doc " + v + ": " + answer(v))
      }



return 0
}

}
