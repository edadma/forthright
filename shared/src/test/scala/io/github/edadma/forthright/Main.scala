package io.github.edadma.forthright

import pprint.pprintln

@main def run(): Unit =
  val env = new Env
  val input =
    """
      |: n 123 . cr ;
      |: p ." asdf" cr ;
      |n p
      |." zxcv" cr
      |3 4 + . cr
      |see n
      |""".stripMargin

  interpret(env, input)
  pprintln(env.dataStack)
  pprintln(env.dataStack.toSeq.reverse)
