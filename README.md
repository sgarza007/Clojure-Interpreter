# Clojure-Interpreter
Created a Clojure Interpreter on Dr. Racket
Saul Garza, Ethan Romney, Erick Perez

Language Practicumm

Language: Clojure

In order to compile and run our interpreter cd into the directory/folder lp2 and then run the command:

lein uberjar 

and once that has run, run the command 

java -jar target/uberjar/lp2-0.1.0-standalone.jar resources/filename.json 

and replace the word “filename” with any testing files provided in the resources folder.

NOTES:

-We have different test cases in the comment block
-We've implemented pairs and an absolute value types as well, however,
we are not given a JSON parser for either since they weren't a mandatory part of the assignment
-We also tried letrec but couldn't fully complete it; the partial implementation is inside the comment block
-To get a field from an object, use command .x where x is the name of field we wanna get out
-We added a pretty-print function which allows us to print out the contents of the expression
-We used two comp bucks :(



