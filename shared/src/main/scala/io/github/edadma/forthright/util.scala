package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec

@tailrec
def skipWhile(r: CharReader, pred: Char => Boolean): CharReader = if pred(r.ch) then skipWhile(r.next, pred) else r

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
