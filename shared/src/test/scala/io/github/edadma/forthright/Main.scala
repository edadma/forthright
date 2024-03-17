package io.github.edadma.forthright

import pprint.pprintln

@main def run(): Unit =
  val env = new Env
  val input =
    """
    hex 12 decimal . cr
    """
  // env.trace = true
  env.interpret(input)
