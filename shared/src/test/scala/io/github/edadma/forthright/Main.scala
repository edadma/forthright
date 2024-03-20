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

    ( 0 factorial . cr
    5 factorial . cr )

    ( create array 2 allot
    3 array !
    array @ . cr
    .s )

    variable count
    : a 0 count ! begin count @ 1+ dup count ! 3 <= while count @ repeat ;
    a
    .s
    """
//  env.trace = true
  env.interpret(input)
