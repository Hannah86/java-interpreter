package javainterpreter.javacompiler

import javainterpreter.javalexer.JavaLexer
import javainterpreter.javaparser.{JavaParser, JavaAST}

object JavaCompiler {
  def apply(code: String) = {
    for {
      tokens <- JavaLexer(code).right
      ast <- JavaParser(tokens).right
    } yield ast
  }
}