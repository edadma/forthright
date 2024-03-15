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

@tailrec
def interpret(env: Env, input: CharReader): Unit =
  val r = skipWhitespace(input)

  if r.more then
    val (r1, s) = consumeWhile(r, !_.isWhitespace)

    val r2 =
      if s.forall(_.isDigit) then
        env.push(s.toInt)
        r1
      else
        env.dictionary get s match
          case None => r.error("word not found")
          case Some(w) =>
            env.mode match
              case Mode.Run =>
                w.run(env)
                r1
              case Mode.Compile => w.compile(env, r1)

    interpret(env, skipWhitespace(r2))
