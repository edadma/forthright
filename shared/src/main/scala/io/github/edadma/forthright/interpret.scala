package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec

@tailrec
def skipWhile(r: CharReader, pred: Char => Boolean): CharReader =
  if pred(r.ch) then skipWhile(r.next, pred) else r

def skipWhitespace(r: CharReader): CharReader = skipWhile(r, _.isWhitespace)

def consumeWhile(r: CharReader, pred: Char => Boolean): (CharReader, String) =
  val buf = new StringBuilder

  @tailrec
  def consumeWhile(r: CharReader): (CharReader, String) =
    if pred(r.ch) && r.more then
      buf += r.ch

      consumeWhile(r.next)
    else (r, buf.toString)

  consumeWhile(r)

def consumeWord(r: CharReader): Either[CharReader, (CharReader, CharReader, String)] =
  val r1 = skipWhitespace(r)

  if r1.more then
    val (end, s) = consumeWhile(r1, !_.isWhitespace)

    Right((r1, end, s))
  else Left(r)

def interpret(env: Env, input: String): Unit = interpret(env, CharReader.fromString(input))

@tailrec
def interpret(env: Env, input: CharReader): Unit =
  consumeWord(input) match
    case Left(_) =>
    case Right((r, r1, s)) =>
      val w =
        if s.forall("-0123456789.eE" contains _) && s.exists(_.isDigit) then NumberWord(s)
        else env.lookup(s, r)
      val r2 =
        env.mode match
          case Mode.Run =>
            env.pos = r

            val r2 = w.run(env, r1)

            env.pos = null
            r2
          case Mode.Compile => w.compile(env, r, r1)

      interpret(env, skipWhitespace(r2))
