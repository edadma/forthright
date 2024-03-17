package io.github.edadma.forthright

import pprint.pprintln

@main def run(): Unit =
  val env = new Env
  val input =
    """
    : a false if ." true" cr then ." done" cr ;
    a
    see a
    see 2dup
    see min
    3 0< .
    """
  env.trace = true
  env.interpret(input)
