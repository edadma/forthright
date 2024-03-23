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
    
    variable counter
    ( : b 0 counter ! begin counter @ 3 < while counter @ . cr counter 1+! repeat ;)
    : b 0 counter ! begin counter @ 3 < if counter @ counter 1+! else exit then again ;
    b .s
    """
//  env.trace = true
  env.interpret(input)
