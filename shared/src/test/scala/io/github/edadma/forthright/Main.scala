package io.github.edadma.forthright

import pprint.pprintln

@main def run(): Unit =
  val env = new Env
  val input =
    """
    : add + ;
    3 4 add . cr
    """

  env.trace = true
  env.interpret(input)
