/**
 * Copyright (c) 2002-2012 "Neo Technology,"
 * Network Engine for Objects in Lund AB [http://neotechnology.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
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
}

