package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader
import pprint.pprintln

@main def run(): Unit =
  val env = new Env
//  val input =
//    """
//      |: n 123 . 10 emit ;
//      |n
//      |." asdf"
//      |""".stripMargin
  val input = """ ." asdf" """

  interpret(env, CharReader.fromString(input))
