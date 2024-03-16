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

def consumeChars(r: CharReader): Either[CharReader, (CharReader, String)] =
  if r.more then Right(consumeWhile(r, !_.isWhitespace))
  else Left(r)

@tailrec
def interpret(env: Env, input: CharReader): Unit =
  val r = skipWhitespace(input)

  consumeChars(r) match
    case Left(_) =>
    case Right((r1, s)) =>
      val w =
        if s.forall(_.isDigit) then NumberWord(s)
        else env.dictionary.getOrElse(s, r.error("word not found"))
      val r2 =
        env.mode match
          case Mode.Run =>
            env.pos = r

            val r2 = w.run(env, r1)

            env.pos = null
            r2
          case Mode.Compile => w.compile(env, r, r1)

      interpret(env, skipWhitespace(r2))
