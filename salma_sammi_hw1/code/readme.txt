1. The file, data.txt in this folder, is the input file containing one document per line and each line is formatted as follows.

 	  Doc<documentid>\tab<token>\space<token>\space<token>...\space<token>

 example :Doc1	breakthrough drug for schizophrenia 


2. For multiple binary AND and OR operators in part 3, my program evaluates any query from left to right and does not support parenthesis. So the query, drug OR treatment AND schizophrenia, will be evaluated as (drug OR treatment) AND schizophrenia


3. Compile with the command:

	sbt compile 


4. Execute with the command:

	sbt 'runMain edu.arizona.cs.AssignmentOne <QUERY>'


  examples to run:
  example 1:	sbt 'runMain edu.arizona.cs.AssignmentOne schizophrenia AND drug'
  example 2:	sbt 'runMain edu.arizona.cs.AssignmentOne breakthrough OR new'
  example 3:	sbt 'runMain edu.arizona.cs.AssignmentOne drug OR treatment AND schizophrenia'
