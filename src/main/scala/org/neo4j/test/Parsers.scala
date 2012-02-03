package org.neo4j.test

import collection.mutable.ListBuffer

trait Parsers {

  def Parser[T](f: Input => ParseResult[T]): Parser[T] = new Parser[T] {
    def apply(in: Input) = f(in)
  }
  
  def failure(message:String) = Parser {
    in => Failure(message, in)
  }

  def rep[T](parser:Parser[T]):Parser[List[T]] = Parser {
    in => {
      val elems = new ListBuffer[T]

      def whileSuccess(where:Input):Success[List[T]]={
        parser(where) match {
          case Success(elem, rest) => {
            elems += elem
            whileSuccess(rest)
          }
          case _ => Success(elems.toList, where)
        }
      }

       whileSuccess(in)
    }
  }

  implicit def accept(e: String): Parser[String] = Parser {
    in => {
      if (e.length() > in.left)
        Failure("Expected [" + e + "], but ran out of input", in)
      else {
        val subSequence = in.text.subSequence(in.offset, in.offset + e.length())
        if (subSequence == e)
          Success(e, in.rest(e.length()))
        else Failure("Expected [" + e + "], but got [" + subSequence + "]", in)

      }
    }
  }

  def parse[T](s: String, parser: Parser[T]): ParseResult[T] = parser(Input(s, 0))
}

case class Input(text: CharSequence, offset: Int) {
  def first = text.charAt(offset)

  def rest = Input(text, offset + 1)

  def left = text.length() - offset

  def rest(chars:Int)=Input(text, offset + chars)
}

case class ~[A,B](a:A,  b:B) {
  override def toString = "(" + a.toString + "~" + b.toString + ")"
}

trait Parser[T] extends (Input => ParseResult[T]) {
  def Parser[T](f: Input => ParseResult[T]): Parser[T] = new Parser[T] {
    def apply(in: Input) = f(in)
  }

  def |[U >: T](other: Parser[U]): Parser[U] = Parser {
    in => apply(in) match {
      case succ: Success[T] => succ
      case _ => other.apply(in)
    }
  }
  
//  def ~[U](other:Parser[U]):Parser[~[T, U]] = ~(this,other)
}
