package io.github.edadma.forthright

import pprint.pprintln

@main def run(): Unit =
  val env = new Env
  val input =
    """
    : a 3 1 do 3 . cr loop ;
    a
    """
  env.trace = true
  env.interpret(input)
