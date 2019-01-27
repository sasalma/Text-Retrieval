import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
//import scala.collection.mutable.LinkedList

object assignment {
    def main(args: Array[String]) {
      if(args.length==0) {println("No query given"); return;}
      if(args.length < 3 || (args.length%2==0)){
        println("Invalid query. Please try again with a valid query")
        return
      }
      //var x = args(0)
      //var y = args(2)

      val invertedIndex = new HashMap[String, List[Int]].withDefaultValue(Nil)


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
                //println("tokens")
                i=0
                for(token <- parts.split(" ")){
                  //println(token)
                  invertedIndex(token) ::= docid.substring(3, docid.length).toInt
                }
              }
            //  println(parts)
            }

      }


      for(v <- invertedIndex.keys){
        //println(v)
        invertedIndex(v) = invertedIndex(v).sorted
        //println(invertedIndex(v))
      }

      var result: List[Int] =List()
      var index = 0;
      result = invertedIndex(args(0))
      for( index <- 1 until (args.length-1) by 2){
        //println("index= "+index + "args(index)= " +args(index))
        if(args(index).toUpperCase()=="AND") result = Intersect(result,invertedIndex(args(index+1)))
         else if(args(index).toUpperCase()=="OR") result = UNION(result,invertedIndex(args(index+1)))
      //  println(result)
      }


      // Print the answer here
      if(result.length==0) println("{}")
      index = 0;
      for( index <- 0 until result.length){
         println( "Doc " + result(index) );
      }

    }


    def Intersect(p1:List[Int] , p2:List[Int] ) : List[Int] = {
      //println(p1)
      var index_p1=0
      var index_p2=0
      var answer: List[Int] =List()
      val answerBuf = new ListBuffer[Int]()
      while (index_p1 < p1.length && index_p2 < p2.length){
        if (p1(index_p1) == p2(index_p2)){
          answerBuf += p1(index_p1)
          index_p1 = index_p1 + 1
          index_p2 = index_p2 + 1
        }
        else if (p1(index_p1) < p2(index_p2)){
          index_p1 = index_p1+ 1
        }
        else index_p2 = index_p2 + 1
      }
      //println("answer for AND")

    //  println(answerBuf)
      answer = answerBuf.toList
      return answer
    //  return 0
    }


    def UNION(p1:List[Int] , p2:List[Int] ) : List[Int] ={
      var index_p1=0
      var index_p2=0
      var answer: List[Int] =List()
      val answerBuf = new ListBuffer[Int]()


    //  println("in OR p1=" + p1)
    //  println("in OR p2="+p2)

      while (index_p1 < p1.length || index_p2 < p2.length){
        if(index_p1 < p1.length && index_p2 < p2.length){
          if (p1(index_p1) == p2(index_p2)){
            answerBuf += p1(index_p1)
            index_p1 = index_p1 + 1
            index_p2 = index_p2 + 1
          }
          else if (p1(index_p1) < p2(index_p2)){
            answerBuf += p1(index_p1)
            index_p1 = index_p1+ 1
          }
          else {
            answerBuf += p2(index_p2)
            index_p2 = index_p2 + 1
          }
        }
        else if (index_p1 < p1.length){
          answerBuf += p1(index_p1)
          index_p1 = index_p1+ 1
        }
        else{
          answerBuf += p2(index_p2)
          index_p2 = index_p2 + 1
        }
      }
    //  println("answer for OR" + answerBuf)
      answer = answerBuf.toList
      return answer
    }
}
