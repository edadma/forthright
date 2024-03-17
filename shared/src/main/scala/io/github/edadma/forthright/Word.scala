package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader

import scala.collection.immutable.ArraySeq

abstract class Word:
  val name: String

  def compile(env: Env, pos: CharReader, r: CharReader): CharReader

  def run(env: Env, pos: CharReader, r: CharReader): CharReader

case class SimpleWrappedWord(pos: CharReader, word: Word) extends Word:
  val name: String = word.name

  def compile(env: Env, pos: CharReader, r: CharReader): CharReader = pos.error("can't compile wrapped word")

  def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    env.pos = pos

    val r1 = word.run(env, pos, r)

    env.pos = null
    r1

abstract class SimpleWord extends Word:
  def compile(env: Env, pos: CharReader, r: CharReader): CharReader =
    env.addToDefinition(SimpleWrappedWord(pos, this))
    r

case class NucleusWord(name: String, action: Env => Unit) extends SimpleWord:
  def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    action(env)
    r

case class DefinedWord(name: String, definition: ArraySeq[Word]) extends SimpleWord:
  def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    env.debug(pos.longErrorText(s">> calling $name").trim)
    env.word = name
    env.call(definition)
    r

case class RuntimeWord(name: String, action: (Env, CharReader) => CharReader) extends Word:
  def compile(env: Env, pos: CharReader, r: CharReader): CharReader = r.error("not allowed here")

  def run(env: Env, pos: CharReader, r: CharReader): CharReader = action(env, r)

case class CompileTimeWord(name: String, action: (Env, CharReader) => CharReader) extends Word:
  def compile(env: Env, pos: CharReader, r: CharReader): CharReader = action(env, r)

  def run(env: Env, pos: CharReader, r: CharReader): CharReader = r.error("not allowed here")

case class NumberWord(name: String) extends SimpleWord:
  val n: Double = name.toDouble

  def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    env push n
    r

case class PrintWord(s: String) extends SimpleWord:
  val name = s"""." s""""

  override def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    print(s)
    r
