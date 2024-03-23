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
    create a 3 , 4 , 5 ,
    : b 0 2 do a i 0> if i + @ . cr else leave then -1 +loop ;
    b
    """
//  env.trace = true
  env.interpret(input)
