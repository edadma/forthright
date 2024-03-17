package io.github.edadma.forthright

import pprint.pprintln

@main def run(): Unit =
  val env = new Env
  val input =
    """
    ." -" 2 SPACES ." -"
    """
  // env.trace = true
  env.interpret(input)
