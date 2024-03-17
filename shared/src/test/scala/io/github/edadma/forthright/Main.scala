package io.github.edadma.forthright

import pprint.pprintln

@main def run(): Unit =
  val env = new Env
  val input =
    """
    ." -" 2 SPACES ." -"
    see spaces
    : asdf ." asdf" ;
    asdf
    words
    """
  // env.trace = true
  env.interpret(input)
