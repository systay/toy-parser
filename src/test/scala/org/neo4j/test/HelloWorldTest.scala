package org.neo4j.test

import org.scalatest.Assertions
import org.junit.Test
import org.junit.Assert._
import org.hamcrest.CoreMatchers._
import org.hamcrest.Matcher
import java.lang.String

class HelloWorldTest extends Parsers with Assertions {
  def a: Parser[String] = "a"

  def b: Parser[String] = "b"

  def abb: Parser[String] = "abb"


  @Test def singleToken() {
    assertSuccess(parse("a", a), equalTo("a"))
    assertFail(parse("b", a))
  }

  @Test def or() {
    assertSuccess(parse("a", a | b), equalTo("a"))
    assertSuccess(parse("b", a | b), equalTo("b"))
    assertFail(parse("c", a | b))
  }

  @Test def rep() {
    assertSuccess(parse("aaa", rep(a)), equalTo(List("a", "a", "a")))
    assertSuccess(parse("abbab", rep(a | b)), equalTo(List("a", "b", "b", "a", "b")))
    assertSuccess(parse("abbaa", rep(abb | a)), equalTo(List("abb", "a", "a")))
    assertSuccess(parse("aaa", rep(b)), equalTo(List[String]()))
  }

  @Test def map() {
    val r = parse("abb", abb.map {
      case "hello" => "YES"
      case _ => "NO"
    })

    println(r)
  }

  private def assertSuccess[T](parsed: ParseResult[T], matcher: Matcher[T]) {
    parsed match {
      case Success(result, rest) => assertThat(result, matcher)
      case _ => throw new Exception("failed")
    }
  }

  private def assertFail(parsed: ParseResult[_]) {
    assertThat(parsed, instanceOf(classOf[Failure]))
    println(parsed)
  }


  //  @Test def twoTokensWithAnd() {
  //    val parser = new Then(new Token('a'), new Token('b'))
  //    val result = parser.apply(new Input("ab", 0))
  //    println(result)
  //  }


  //  class Token(token: Char) extends Parser[String] {
  //    def apply(v1: Input) = if (v1.first == token) Success(token.toString, v1.rest) else Failure("wut", v1)
  //  }
  //
  //  case class Then[T, R](a: Parser[T], b: Parser[R]) extends Parser[(T, R)] {
  //    def apply(value: Input) = a(value) match {
  //      case x: Failure => x
  //      case Success(aVal, temp) => b(temp) match {
  //        case Success(bVal, rest) => Success((aVal, bVal), rest)
  //        case x: Failure => x
  //      }
  //    }
  //  }
}

