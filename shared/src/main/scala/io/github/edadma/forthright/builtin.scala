package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader

import scala.language.postfixOps

val builtin =
  List(
    //
    // Nucleus words
    NucleusWord("dup", env => env push env.dataStack.top),
    NucleusWord("swap", env => env.pushAll(env npop 2 reverse)),
    NucleusWord("over", env => env.push(env.dataStack(1))),
    NucleusWord("pick", env => env.push(env.dataStack(env.popi - 1))),
    NucleusWord(".", env => print(env.pop)),
    NucleusWord("emit", env => print(env.popi.toChar)),
    NucleusWord("+", env => env.push(env npopn 2 sum)),
    //
    // Compiler words
    CompilerWord(
      ":",
      { (env, r) =>
        val r1 = skipWhitespace(r)

        consumeChars(r1) match
          case Left(r2) => r1.error("unclosed definition")
          case Right((r2, s)) =>
            env.openDefinition(s)
            r2
      },
    ),
    CompileModeWord(
      ";",
      { (env, r) =>
        env.closeDefinition()
        r
      },
    ),
    new Word {
      val name = ".\""

      def compile(env: Env, pos: CharReader, r: CharReader): CharReader =
        val (r1, s) = consumeWhile(skipWhitespace(r), _ != '"')

        PrintWord(s).compile(env, pos, r1.next)

      def run(env: Env, r: CharReader): CharReader =
        val (r1, s) = consumeWhile(skipWhitespace(r), _ != '"')

        print(s)
        r1.next
    },
  )
