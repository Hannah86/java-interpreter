package javainterpreter.javacompiler

sealed trait JavaCompilationError

case class JavaLexerError(location: Location, msg: String) extends JavaCompilationError
case class JavaParserError(location: Location, msg: String) extends JavaCompilationError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}
