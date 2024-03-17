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
    """
  env.trace = true
  env.interpret(input)
