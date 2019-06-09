package javainterpreter

import org.scalatest.{FlatSpec, Matchers}
import javainterpreter.javacompiler.{Location, JavaCompiler, JavaParserError}
import javainterpreter.javalexer._
import javainterpreter.javaparser._
import javainterpreter.javaparser.JavaInterpreter._
import scala.collection.mutable.Map
import scala.collection.mutable.ListMap
import scala.collection.mutable.HashMap

class JavaInterpreterSpec extends FlatSpec with Matchers {

  val code = 
         """
         String a = "e";
         String b = "e";
         int c = 8;
         int d = 10;
         if ("e" == "e"){
            c = 1000;
            d = 1000;
         }
         """
  val code1 = 
         """
         int b = 5;

         """

  val code2 = 
    """
    int a             = 33;
    String     b = "3";
    double  c = 4.5;
    if (c >= 4.4){ if (a > 55){ int e = 100;
      }
      int c = 10;
    }
    """

  val code3 = 
    //int a = 3;
    // int b = 0;
    """
    int a = 3;
    int c = 0;
    if (a < 4){
      b = 5;
      c = 10;
    }else{
      b = 10;

    }


    """
  val code4 = 
    """
    String a = "hiii";
    int b = 3;

    switch (a){
      case "hi":
        b=1;
        break;
      case "hii":
        b=2;
        break;
      case "hiii":
        b=0;
        break;
    }
    System.out.println("Value of b is:");
    System.out.println(b);

    """

  val code5 = 
    """
    int sum1 = 0;
    for(int i = 0; i < 5; i++){
      sum1 = sum1 + i;
 
    }
    System.out.println("The sum of 1 to 5 (for-loop):");
    System.out.println(sum1);
    
    int a = 1;
    int sum2 = 0;
    while(a < 5){
      sum2 = sum2 + a;
      a = a + 1;
    }
    System.out.println("The sum of 1 to 5 (while-loop):");
    System.out.println(sum2);
    """
    // String aa = "Hello";
    // String bb = "World";
    // String cc = aa + bb;
    // String a = "Hello";
    // String b = "World";
    // String c = a + b;
  //println(JavaLexer.apply(code))
  


val code6 = 
  """
 int    a          = 33;
    String     b = "3";
    double  c = 4.5; if (c >= 4.4) { int c = 10;
}
"""



val numstore = new HashMap[String, Double]
val strstore = new HashMap[String, String]
val store = new Env(numstore, strstore)


// val store_test: NumEnv = Map()
// val store_string: StrEnv = Map()
// val env: Env = Map()
// env("number") = store_test
// env("string") = store_string


val result = JavaCompiler(code6)
println(JavaCompiler(code6))

println( result match {
  case Right(x) => {
    evalJavaAST(x.asInstanceOf[JavaAST], store).number
    //evalJavaAST(x.asInstanceOf[JavaAST], store).string
  }
  case Left(x) => {
    evalJavaAST(x.asInstanceOf[JavaAST], store).number
    //evalJavaAST(x.asInstanceOf[JavaAST], store).string
  }
})


}











