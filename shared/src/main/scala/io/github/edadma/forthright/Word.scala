package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader

import scala.collection.immutable.ArraySeq

abstract class Word:
  val name: String

  def compile(env: Env, r: CharReader): CharReader

  def run(env: Env): Unit

abstract class SimpleWord extends Word:
  def compile(env: Env, r: CharReader): CharReader =
    env.addToDefinition(this)
    r

case class NucleusWord(name: String, action: Env => Unit) extends SimpleWord:
  def run(env: Env): Unit = action(env)

case class Definition(name: String, definition: ArraySeq[Word]) extends SimpleWord:
  def run(env: Env): Unit =
    env.pc = 0

    while env.pc < definition.length do
      definition(env.pc).run(env)
      env.pc += 1

case class CompilerWord(name: String, action: Env => Unit) extends Word:
  def compile(env: Env, r: CharReader): CharReader = r.error("not allowed here")

  def run(env: Env): Unit = action(env)
