package io.github.edadma.forthright

import scala.language.postfixOps

val builtin =
  List(
    NuclearWord("dup", env => env push env.dataStack.top),
    NuclearWord("swap", env => env.pushAll(env npop 2 reverse)),
    NuclearWord("over", env => env.push(env.dataStack(1))),
    NuclearWord("pick", env => env.push(env.dataStack(env.popi - 1))),
    NuclearWord(".", env => print(env.pop)),
    NuclearWord("emit", env => print(env.popi.toChar)),
  )
