package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader

import scala.collection.immutable.ArraySeq

abstract class Word:
  val name: String

  def compile(env: Env, r: CharReader): CharReader

  def run(env: Env, r: CharReader): CharReader

abstract class SimpleWord extends Word:
  def compile(env: Env, r: CharReader): CharReader =
    env.addToDefinition(this)
    r

case class NucleusWord(name: String, action: Env => Unit) extends SimpleWord:
  def run(env: Env, r: CharReader): CharReader =
    action(env)
    r

case class Definition(name: String, definition: ArraySeq[Word]) extends SimpleWord:
  def run(env: Env, r: CharReader): CharReader =
    env.pc = 0

    while env.pc < definition.length do
      val word =
        definition(env.pc) match
          case WrappedWord(pos, word) =>
            println(word.name)
            env.pos = pos
            word
          case word =>
            env.pos = null
            word

      word.run(env, null)
      env.pc += 1

    r

case class CompilerWord(name: String, action: (Env, CharReader) => CharReader) extends Word:
  def compile(env: Env, r: CharReader): CharReader = r.error("not allowed here")

  def run(env: Env, r: CharReader): CharReader = action(env, r)

case class CompileModeWord(name: String, action: (Env, CharReader) => CharReader) extends Word:
  def compile(env: Env, r: CharReader): CharReader = action(env, r)

  def run(env: Env, r: CharReader): CharReader = r.error("not allowed here")

case class NumberWord(name: String) extends SimpleWord:
  val n: Int = name.toInt

  def run(env: Env, r: CharReader): CharReader =
    env push n
    r

case class PrintWord(s: String) extends SimpleWord:
  val name = s"""." s""""

  override def run(env: Env, r: CharReader): CharReader =
    print(s)
    r

case class WrappedWord(pos: CharReader, word: Word) extends Word:
  val name: String = word.name

  def compile(env: Env, r: CharReader): CharReader = word.compile(env, r)

  def run(env: Env, r: CharReader): CharReader = word.run(env, r)
