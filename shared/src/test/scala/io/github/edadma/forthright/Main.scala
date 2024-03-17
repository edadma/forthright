package io.github.edadma.forthright

import pprint.pprintln

@main def run(): Unit =
  val env = new Env
  val input =
    """
    variable a
    see a
    words
    a .s
    @ .s drop
    123 a !
    a @ . cr
    see a
    a .s
    """
  // env.trace = true
  env.interpret(input)
