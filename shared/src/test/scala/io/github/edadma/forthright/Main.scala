package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader
import pprint.pprintln

@main def run(): Unit =
  val env = new Env
  val input =
    """
      |123 .
      |""".stripMargin

  interpret(env, CharReader.fromString(input))
