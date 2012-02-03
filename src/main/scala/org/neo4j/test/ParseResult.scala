package org.neo4j.test

abstract sealed class ParseResult[+T]

case class Success[T](result:T, rest:Input) extends ParseResult[T]
case class Failure(message:String, pos:Input) extends ParseResult[Nothing]