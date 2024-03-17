package io.github.edadma.forthright

import pprint.pprintln

@main def run(): Unit =
  val env = new Env
  val input =
    """
    : add + ( asdf) ;
    3 4 add . cr
    ( asdf)
    ." zxcv" cr
    """

  env.trace = true
  env.interpret(input)
