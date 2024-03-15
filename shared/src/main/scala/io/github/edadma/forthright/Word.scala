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

case class NuclearWord(name: String, action: Env => Unit) extends SimpleWord:
  def run(env: Env): Unit = action(env)

case class Definition(name: String, definition: ArraySeq[Word]) extends SimpleWord:
  def run(env: Env): Unit =
    var pc = 0

    while pc < definition.length do
      definition(pc).run(env)
      pc += 1
