# java-interpreter
This project is a basic Java interpreter using Scala programming language.
To run the project, first, download the project.
```
git clone https://github.com/Hannah86/java-interpreter.git
```
Then run "sbt" in the project directory.
```
sbt
```
After the compiling, input "test" to see the result.
```
test
```
All the test cases are in "java-interpreter/src/test/scala/javainterpreter/JavaInterpreterSpec.scala". And to test different Java code, change the parameter of "JavaCompiler" object.
```
val result = JavaCompiler(code6)
println(JavaCompiler(code6))
```
This will show the AST of the Java code, the above code will show the result of the Java code.
```
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
```
The default environment of the result is the type of number. If you want to see the string environment, just comment the first "evalJavaAST" code and use the second line of "evalJavaAST".
