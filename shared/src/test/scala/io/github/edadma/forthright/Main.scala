package io.github.edadma.forthright

import pprint.pprintln

@main def run(): Unit =
  val env = new Env
  val input =
    """
    : FACTORIAL ( n -- n! )
      DUP 1 > IF
        1+ 1 SWAP 1 DO I * LOOP
      ELSE
        DROP 1
      THEN ;

    0 factorial . cr
    5 factorial . cr

    variable count
    : a 1 count ! begin count @ dup 1+ count ! dup 3 = until ;
    a
    .s
    """
  // env.trace = true
  env.interpret(input)
