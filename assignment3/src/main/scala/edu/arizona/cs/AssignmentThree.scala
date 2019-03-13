package edu.arizona.cs

import scala.io.Source

import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.RAMDirectory;

object AssignmentThree {
    def main(args: Array[String]) {

//println("test print")
        // 0. Specify the analyzer for tokenizing text.
        //    The same analyzer should be used for indexing and searching
        val analyzer = new StandardAnalyzer();

        // 1. create the index
        val index = new RAMDirectory();

        val config = new IndexWriterConfig(analyzer);

        val w = new IndexWriter(index, config);
        val filename = "data.txt"
        for (line <- Source.fromFile(filename).getLines) {
          var words = line.split("\t")
          addDoc(w, words(1), words(0));
        }
        w.close();

        if(args.length == 0) return; // no query given

        // 2. query
        //String querystr = args.length > 0 ? args(0) : "lucene";
        var querystr = ""
        querystr = querystr + args(0)
        for(i<-1 until args.length){
          querystr = querystr + " " + args(i)
        }

        // the "text" arg specifies the default field to use
        // when no field is explicitly specified in the query.
        val q = new QueryParser("text", analyzer).parse(querystr);

        // 3. search
        val hitsPerPage = 10;
        val reader = DirectoryReader.open(index);
        val searcher = new IndexSearcher(reader);
        val docs = searcher.search(q, hitsPerPage);
        //ScoreDoc[] hits = docs.scoreDocs;
	      val hits = docs.scoreDocs;

        // 4. display results
        System.out.println("Found " + hits.length + " hits.");
        //println(hits);

        var i = 0
        for(i<-0 until hits.length) {
            val docId = hits(i).doc;
            val d = searcher.doc(docId);
            //System.out.println((i + 1) + ". " + d.get("docid") + "\t" + d.get("text"));
            System.out.println((i + 1) + ". " + d.get("docid") + "\t" + hits(i).score);
        }

        // reader can only be closed when there
        // is no need to access the documents any more.
        reader.close();

    }

    def addDoc(w:IndexWriter, text:String , docid:String ) {
        val doc = new Document();
        doc.add(new TextField("text", text, Field.Store.YES));

        // use a string field for docid because we don't want it tokenized
        doc.add(new StringField("docid", docid, Field.Store.YES));
        w.addDocument(doc);
    }
}
