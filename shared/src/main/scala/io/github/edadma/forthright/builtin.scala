package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader

import scala.language.postfixOps

val builtin =
  List(
    //
    // Nucleus words
    NucleusWord("DUP", (env, _) => env push env.dataStack.top),
    NucleusWord("?DUP", (env, _) => if env.dataStack.top != 0 then env push env.dataStack.top),
    NucleusWord("DROP", (env, _) => env.pop),
    NucleusWord("SWAP", (env, _) => env.pushAll(env npop 2 reverse)),
    NucleusWord("ROT", (env, _) => env.pushAll(env.npop(2) :+ env.pop)),
    NucleusWord("ROLL", (env, _) => env.pushAll(env.npop(env.popp - 1) :+ env.pop)),
    NucleusWord("OVER", (env, _) => env.push(env.dataStack(1))),
    NucleusWord("PICK", (env, _) => env.push(env.dataStack(env.popp - 1))),
    NucleusWord("DEPTH", (env, _) => env.pushn(env.dataStack.length)),
    NucleusWord(".", (env, _) => print(env.display(env.pop))),
    NucleusWord("?", (env, _) => print(env.display(env.pop.asInstanceOf[Address].value))),
    NucleusWord("EMIT", (env, _) => print(env.popn.toChar)),
    NucleusWord("CR", (_, _) => println),
    NucleusWord("SPACE", (_, _) => print(" ")),
    NucleusWord("+", (env, _) => env.push(env npopn 2 sum)),
    NucleusWord("*", (env, _) => env.push(env npopn 2 product)),
    NucleusWord("-", (env, _) => env.push(env execn2 (_ - _))),
    NucleusWord("/", (env, _) => env.push(env execn2 (_ / _))),
    NucleusWord("MOD", (env, _) => env.push(env execn2 (_ % _))),
    NucleusWord("NEGATE", (env, _) => env.push(-env.popn)),
    NucleusWord("I", (env, _) => env.push(env.returnStack.top.asInstanceOf[Return.Loop].index)),
    NucleusWord("J", (env, _) => env.push(env.returnStack(1).asInstanceOf[Return.Loop].index)),
    NucleusWord("<", (env, _) => env.push(env execn2 (_ < _))),
    NucleusWord(">", (env, _) => env.push(env execn2 (_ > _))),
    NucleusWord("<=", (env, _) => env.push(env execn2 (_ <= _))),
    NucleusWord(">=", (env, _) => env.push(env execn2 (_ >= _))),
    NucleusWord("=", (env, _) => env.push(env.exec2[Any](_ == _))),
    NucleusWord("<>", (env, _) => env.push(env.exec2[Any](_ != _))),
    NucleusWord(".S", (env, _) => println(env.stack)),
    NucleusWord("WORDS", (env, _) => println(env.dictionary.keys.toList.reverse mkString " ")),
    NucleusWord("@", (env, _) => env push env.pop.asInstanceOf[Address].value),
    NucleusWord("!", (env, _) => env.pop.asInstanceOf[Address].value = env.pop),
    NucleusWord("BASE", (env, _) => env push env),
    NucleusWord("DECIMAL", (env, _) => env.base = 10),
    NucleusWord("HEX", (env, _) => env.base = 16),
    NucleusWord("OCTAL", (env, _) => env.base = 8),
    NucleusWord("BINARY", (env, _) => env.base = 2),
    NucleusWord("NOT", (env, _) => env push !env.dataStack.pop.asInstanceOf[Boolean]),
    NucleusWord(">R", (env, _) => env.returnStack.push(Return.Data(env.pop))),
    NucleusWord("R>", (env, _) => env push env.returnStack.pop.asInstanceOf[Return.Data].value),
    NucleusWord("R@", (env, _) => env push env.returnStack.top.asInstanceOf[Return.Data].value),
    NucleusWord("EXECUTE", (env, pos) => env.pop.asInstanceOf[Word].run(env, pos, null)),
    NucleusWord(
      "OR",
      (env, _) =>
        env.push(env.exec2[Any]({
          case (a: Boolean, b: Boolean)                         => a || b
          case (a: Double, b: Double) if a.isWhole && b.isWhole => (a.toInt | b.toInt).toDouble
          case (_, _)                                           => env.error("expected boolean or integer arguments")
        })),
    ),
    NucleusWord(
      "XOR",
      (env, _) =>
        env.push(env.exec2[Any]({
          case (a: Boolean, b: Boolean)                         => a ^ b
          case (a: Double, b: Double) if a.isWhole && b.isWhole => (a.toInt ^ b.toInt).toDouble
          case (_, _)                                           => env.error("expected boolean or integer arguments")
        })),
    ),
    NucleusWord(
      "AND",
      (env, _) =>
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
      { (env, _, r) =>
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
      { (env, _, r) =>
        consumeWord(r) match
          case Left(r2) => r2.error("constant name expected")
          case Right((_, r2, s)) =>
            env.addToDictionary(Seq(ConstantWord(s.toUpperCase, env.pop)))
            r2
      },
    ),
    RuntimeWord(
      "VARIABLE",
      { (env, _, r) =>
        consumeWord(r) match
          case Left(r2) => r2.error("variable name expected")
          case Right((_, r2, s)) =>
            env.addToDictionary(Seq(VariableWord(s.toUpperCase)))
            r2
      },
    ),
    RuntimeWord(
      "CREATE",
      { (env, _, r) =>
        consumeWord(r) match
          case Left(r2) => r2.error("array name expected")
          case Right((_, r2, s)) =>
            env.addToDictionary(Seq(ArrayWord(s.toUpperCase)))
            r2
      },
    ),
    RuntimeWord(
      "ALLOT",
      { (env, pos, r) =>
        env.dictionary.last._2 match
          case w: ArrayWord => w.array.appendAll(Iterator.fill(env.popp)(null))
          case _            => pos.error("can only ALLOT after CREATE")
        r
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
          case If(idx)   => env.buf(idx) = FalseBranchWord("IF", env.buf.length)
          case Else(idx) => env.buf(idx) = BranchWord("ELSE", env.buf.length)
          case _         => env.error("THEN without corresponding IF")

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
      "WHILE",
      { (env, r) =>
        if env.dataStack.isEmpty then env.error("WHILE without corresponding BEGIN")

        env.pop match
          case Begin(idx) =>
            env push While(idx, env.buf.length)
            env.addToDefinition(null)
            r
          case _ => env.error("WHILE without corresponding BEGIN")
      },
    ),
    CompileTimeWord(
      "REPEAT",
      { (env, r) =>
        if env.dataStack.isEmpty then env.error("REPEAT without corresponding WHILE")

        env.pop match
          case While(begin, idx) =>
            env.buf(idx) = FalseBranchWord("WHILE", env.buf.length)
            env.addToDefinition(BranchWord("REPEAT", begin))
            r
          case _ => env.error("REPEAT without corresponding WHILE")
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
      val name = "'"

      def compile(env: Env, pos: CharReader, r: CharReader): CharReader =
        consumeWord(r) match
          case Left(r2) => r2.error("word name expected")
          case Right((r1, r2, s)) =>
            env.lookup(s, r1) match
              case w: Word =>
                TickWord(w).compile(env, pos, r2)
                r2
              case _ => r1.error("unknown word")

      def run(env: Env, pos: CharReader, r: CharReader): CharReader =
        consumeWord(r) match
          case Left(r2) => r2.error("word name expected")
          case Right((r1, r2, s)) =>
            env.lookup(s, r1) match
              case w: Word =>
                env push w
                r2
              case _ => r1.error("unknown word")
    },
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
    LiteralWord("TRUE", true),
    LiteralWord("FALSE", false),
    LiteralWord("NULL", null),
    RuntimeWord(
      "SEE",
      { (env, _, r) =>
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
