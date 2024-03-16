package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader

import scala.language.postfixOps

val builtin =
  List(
    //
    // Nucleus words
    NucleusWord("DUP", env => env push env.dataStack.top),
    NucleusWord("SWAP", env => env.pushAll(env npop 2 reverse)),
    NucleusWord("ROT", env => env.pushAll(env.npop(2) :+ env.pop)),
    NucleusWord("OVER", env => env.push(env.dataStack(1))),
    NucleusWord("PICK", env => env.push(env.dataStack(env.popi - 1))),
    NucleusWord(".", env => print(env.pop)),
    NucleusWord("EMIT", env => print(env.popi.toChar)),
    NucleusWord("CR", _ => println),
    NucleusWord("SPACE", _ => print(" ")),
    NucleusWord("+", env => env.push(env npopn 2 sum)),
    NucleusWord("*", env => env.push(env npopn 2 product)),
    NucleusWord("-", env => env.push(env execn2 (_ - _))),
    NucleusWord("/", env => env.push(env execn2 (_ / _))),
    //
    // Compiler words
    RuntimeWord(
      ":",
      { (env, r) =>
        consumeWord(r) match
          case Left(r2) => r2.error("word name expected")
          case Right((r1, r2, s)) =>
            env.openDefinition(s)
            r2
      },
    ),
    CompileTimeWord(
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
    //
    // non Forth-79 words
    RuntimeWord(
      "SEE",
      { (env, r) =>
        consumeWord(r) match
          case Left(r2) => r2.error("word name expected")
          case Right((r1, r2, s)) =>
            env.lookup(s, r1) match
              case Definition(name, definition) =>
                println(s": $name ${definition map (_.name) mkString " "} ;")
              case _ => r1.error("not a user-defined word")
            r2
      },
    ),
  )
