package io.github.edadma.forthright

import pprint.pprintln

@main def run(): Unit =
  val env = new Env
  val input =
    """
    123 constant a
    see a
    : b a 1+ . cr ;
    see b
    b
    words
    a .s
    """
  // env.trace = true
  env.interpret(input)
