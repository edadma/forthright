package io.github.edadma.forthright

import scala.language.postfixOps

val builtin =
  List(
    NucleusWord("dup", env => env push env.dataStack.top),
    NucleusWord("swap", env => env.pushAll(env npop 2 reverse)),
    NucleusWord("over", env => env.push(env.dataStack(1))),
    NucleusWord("pick", env => env.push(env.dataStack(env.popi - 1))),
    NucleusWord(".", env => print(env.pop)),
    NucleusWord("emit", env => print(env.popi.toChar)),
    //
    // Compiler words
    CompilerWord(
      ":",
      { (env, r) =>
        consumeChars(r) match
          case Left(r) => r.error("unclosed definition")
          case Right((r, s)) =>
            env.openDefinition(s)
            r
      },
    ),
    CompileModeWord(
      ";",
      { (env, r) =>
        env.closeDefinition()
        r
      },
    ),
  )
