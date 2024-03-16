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

def consumeChars(input: CharReader): Either[CharReader, (CharReader, String)] =
  val r = skipWhitespace(input)

  if r.more then Right(consumeWhile(r, !_.isWhitespace))
  else Left(r)

@tailrec
def interpret(env: Env, input: CharReader): Unit =
  consumeChars(input) match
    case Left(_) =>
    case Right((r, s)) =>
      val w =
        if s.forall(_.isDigit) then NumberWord(s)
        else env.dictionary.getOrElse(s, r.error("word not found"))
      val r1 =
        env.mode match
          case Mode.Run     => w.run(env, r)
          case Mode.Compile => w.compile(env, r)

      interpret(env, skipWhitespace(r1))
