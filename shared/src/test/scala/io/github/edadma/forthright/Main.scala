package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader
import pprint.pprintln

@main def run(): Unit =
  val env = new Env
  val input =
    """
      |: nl 10 emit ;
      |: n 123 . nl ;
      |: p ." asdf" nl ;
      |n p
      |." zxcv" nl
      | 4 + .
      |""".stripMargin

  interpret(env, CharReader.fromString(input))
