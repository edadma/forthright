package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader

import scala.language.postfixOps

val builtin =
  List(
    //
    // Nucleus words
    NucleusWord("DUP", env => env push env.dataStack.top),
    NucleusWord("?DUP", env => if env.dataStack.top != 0 then env push env.dataStack.top),
    NucleusWord("DROP", env => env.pop),
    NucleusWord("SWAP", env => env.pushAll(env npop 2 reverse)),
    NucleusWord("ROT", env => env.pushAll(env.npop(2) :+ env.pop)),
    NucleusWord("ROLL", env => env.pushAll(env.npop(env.popp - 1) :+ env.pop)),
    NucleusWord("OVER", env => env.push(env.dataStack(1))),
    NucleusWord("PICK", env => env.push(env.dataStack(env.popp - 1))),
    NucleusWord("DEPTH", env => env.pushn(env.dataStack.length)),
    NucleusWord(".", env => print(env.display(env.pop))),
    NucleusWord("?", env => print(env.display(env.pop.asInstanceOf[Address].value))),
    NucleusWord("EMIT", env => print(env.popn.toChar)),
    NucleusWord("CR", _ => println),
    NucleusWord("SPACE", _ => print(" ")),
    NucleusWord("+", env => env.push(env npopn 2 sum)),
    NucleusWord("*", env => env.push(env npopn 2 product)),
    NucleusWord("-", env => env.push(env execn2 (_ - _))),
    NucleusWord("/", env => env.push(env execn2 (_ / _))),
    NucleusWord("MOD", env => env.push(env execn2 (_ % _))),
    NucleusWord("NEGATE", env => env.push(-env.popn)),
    NucleusWord("I", env => env.push(env.returnStack.top.asInstanceOf[Return.Loop].index)),
    NucleusWord("J", env => env.push(env.returnStack(1).asInstanceOf[Return.Loop].index)),
    NucleusWord("<", env => env.push(env execn2 (_ < _))),
    NucleusWord(">", env => env.push(env execn2 (_ > _))),
    NucleusWord("<=", env => env.push(env execn2 (_ <= _))),
    NucleusWord(">=", env => env.push(env execn2 (_ >= _))),
    NucleusWord("=", env => env.push(env.exec2[Any](_ == _))),
    NucleusWord("<>", env => env.push(env.exec2[Any](_ != _))),
    NucleusWord(".S", env => println(env.stack)),
    NucleusWord("WORDS", env => println(env.dictionary.keys.toList.reverse mkString " ")),
    NucleusWord("@", env => env push env.pop.asInstanceOf[Address].value),
    NucleusWord("!", env => env.pop.asInstanceOf[Address].value = env.pop),
    NucleusWord("BASE", env => env push env),
    NucleusWord("DECIMAL", _.base = 10),
    NucleusWord("HEX", _.base = 16),
    NucleusWord("OCTAL", _.base = 8),
    NucleusWord("BINARY", _.base = 2),
    NucleusWord("NOT", env => env push !env.dataStack.pop.asInstanceOf[Boolean]),
    NucleusWord(">R", env => env.returnStack.push(Return.Data(env.pop))),
    NucleusWord("R>", env => env push env.returnStack.pop.asInstanceOf[Return.Data].value),
    NucleusWord("R@", env => env push env.returnStack.top.asInstanceOf[Return.Data].value),
    NucleusWord(
      "OR",
      env =>
        env.push(env.exec2[Any]({
          case (a: Boolean, b: Boolean)                         => a || b
          case (a: Double, b: Double) if a.isWhole && b.isWhole => (a.toInt | b.toInt).toDouble
          case (_, _)                                           => env.error("expected boolean or integer arguments")
        })),
    ),
    NucleusWord(
      "XOR",
      env =>
        env.push(env.exec2[Any]({
          case (a: Boolean, b: Boolean)                         => a ^ b
          case (a: Double, b: Double) if a.isWhole && b.isWhole => (a.toInt ^ b.toInt).toDouble
          case (_, _)                                           => env.error("expected boolean or integer arguments")
        })),
    ),
    NucleusWord(
      "AND",
      env =>
        env.push(env.exec2[Any]({
          case (a: Boolean, b: Boolean)                         => a && b
          case (a: Double, b: Double) if a.isWhole && b.isWhole => (a.toInt & b.toInt).toDouble
          case (_, _)                                           => env.error("expected boolean or integer arguments")
        })),
    ),
    //
    // Compiler words
    RuntimeWord(
      ":",
      { (env, r) =>
        consumeWord(r) match
          case Left(r1) => r1.error("word name expected")
          case Right((r1, r2, s)) =>
            if env.defined(s) then r1.error("duplicate definition")

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
    RuntimeWord(
      "CONSTANT",
      { (env, r) =>
        consumeWord(r) match
          case Left(r2) => r2.error("constant name expected")
          case Right((_, r2, s)) =>
            env.addToDictionary(Seq(ConstantWord(s.toUpperCase, env.pop)))
            r2
      },
    ),
    RuntimeWord(
      "VARIABLE",
      { (env, r) =>
        consumeWord(r) match
          case Left(r2) => r2.error("variable name expected")
          case Right((_, r2, s)) =>
            env.addToDictionary(Seq(VariableWord(s.toUpperCase)))
            r2
      },
    ),
    CompileTimeWord(
      "IF",
      { (env, r) =>
        env push If(env.buf.length)
        env.addToDefinition(null)
        r
      },
    ),
    CompileTimeWord(
      "ELSE",
      { (env, r) =>
        if env.dataStack.isEmpty then env.error("ELSE without corresponding IF")

        env.pop match
          case If(idx) =>
            env push Else(env.buf.length)
            env.buf(idx) = FalseBranchWord("IF", env.buf.length)
            env.addToDefinition(null)
          case _ => env.error("ELSE without corresponding IF")

        r
      },
    ),
    CompileTimeWord(
      "THEN",
      { (env, r) =>
        if env.dataStack.isEmpty then env.error("THEN without corresponding IF")

        env.pop match
          case If(idx) => env.buf(idx) = FalseBranchWord("IF", env.buf.length)
          case Else(idx)            => env.buf(idx) = BranchWord("ELSE", env.buf.length)
          case _                         => env.error("THEN without corresponding IF")

        env.addToDefinition(NoopWord("THEN"))
        r
      },
    ),
    CompileTimeWord(
      "BEGIN",
      { (env, r) =>
        env push Begin(env.buf.length)
        env.addToDefinition(NoopWord("BEGIN"))
        r
      },
    ),
    CompileTimeWord(
      "UNTIL",
      { (env, r) =>
        if env.dataStack.isEmpty then env.error("UNTIL without corresponding BEGIN")

        env.pop match
          case Begin(idx) =>
            env.addToDefinition(FalseBranchWord("UNTIL", idx))
            r
          case _ => env.error("UNTIL without corresponding BEGIN")
      },
    ),
    CompileTimeWord(
      "DO",
      { (env, r) =>
        env push Do(env.buf.length)
        env.addToDefinition(DoWord)
        r
      },
    ),
    CompileTimeWord(
      "LOOP",
      { (env, r) =>
        if env.dataStack.isEmpty then env.error("LOOP without corresponding DO")

        env.pop match
          case Do(idx) =>
            env.addToDefinition(LoopWord("LOOP", idx, _ => 1))
            r
          case _ => env.error("LOOP without corresponding DO")
      },
    ),
    CompileTimeWord(
      "+LOOP",
      { (env, r) =>
        if env.dataStack.isEmpty then env.error("+LOOP without corresponding DO")

        env.pop match
          case Do(idx) =>
            env.addToDefinition(LoopWord("+LOOP", idx, _.popn))
            r
          case _ => env.error("+LOOP without corresponding DO")
      },
    ),
    new Word {
      val name = ".\""

      def compile(env: Env, pos: CharReader, r: CharReader): CharReader =
        val (r1, s) = consumeWhile(skipWhitespace(r), _ != '"')

        PrintWord(s).compile(env, pos, r1.next)

      def run(env: Env, pos: CharReader, r: CharReader): CharReader =
        val (r1, s) = consumeWhile(skipWhitespace(r), _ != '"')

        print(s)
        r1.next
    },
    new Word {
      val name = "("

      def compile(env: Env, pos: CharReader, r: CharReader): CharReader =
        val (r1, s) = consumeWhile(skipWhitespace(r), _ != ')')

        NoopWord(s"( $s)").compile(env, pos, r1.next)

      def run(env: Env, pos: CharReader, r: CharReader): CharReader =
        val r1 = skipWhile(skipWhitespace(r), _ != ')')

        r1.next
    },
    //
    // non Forth-79 words
    NucleusWord("TRUE", _ push true),
    NucleusWord("FALSE", _ push false),
    NucleusWord("NULL", _ push null),
    RuntimeWord(
      "SEE",
      { (env, r) =>
        consumeWord(r) match
          case Left(r2) => r2.error("word name expected")
          case Right((r1, r2, s)) =>
            env.lookup(s, r1) match
              case DefinedWord(name, definition) => println(s": $name ${definition map (_.name) mkString " "} ;")
              case ConstantWord(name, value)     => println(s"${env.display(value)} CONSTANT $name")
              case v @ VariableWord(name)        => println(s"VARIABLE $name ( ${env.display(v.value)} )")
              case _                             => r1.error("not a user-defined word")

            r2
      },
    ),
  )
