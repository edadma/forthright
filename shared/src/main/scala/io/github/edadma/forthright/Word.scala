package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

abstract class Word:
  val name: String

  def compile(env: Env, pos: CharReader, r: CharReader): CharReader

  def run(env: Env, pos: CharReader, r: CharReader): CharReader

  override def toString: String = s"<word: $name>"

case class WrapperWord(pos: CharReader, word: Word) extends Word:
  val name: String = word.name

  def compile(env: Env, pos: CharReader, r: CharReader): CharReader = pos.error("can't compile wrapped word")

  def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    env.pos = pos

    val r1 = word.run(env, pos, r)

    env.pos = null
    r1

abstract class SimpleWord extends Word:
  def compile(env: Env, pos: CharReader, r: CharReader): CharReader =
    env.addToDefinition(WrapperWord(pos, this))
    r

case class NucleusWord(name: String, action: (Env, CharReader) => Unit) extends SimpleWord:
  def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    action(env, pos)
    r

case class FunctionWord(name: String, f: Double => Double) extends SimpleWord:
  def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    env push f(env.popn)
    r

case class Function2Word(name: String, f: (Double, Double) => Double) extends SimpleWord:
  def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    val Seq(a, b) = env npopn 2

    env push f(a, b)
    r

case class DefinedWord(name: String, definition: ArraySeq[Word]) extends SimpleWord:
  def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    env.debug(if pos ne null then pos.longErrorText(s">> calling $name").trim else s">> calling $name")
    env.word = name
    env.call(definition)
    r

case class RuntimeWord(name: String, action: (Env, CharReader, CharReader) => CharReader) extends Word:
  def compile(env: Env, pos: CharReader, r: CharReader): CharReader = r.error("not allowed here")

  def run(env: Env, pos: CharReader, r: CharReader): CharReader = action(env, pos, r)

case class CompileTimeWord(name: String, action: (Env, CharReader) => CharReader) extends Word:
  def compile(env: Env, pos: CharReader, r: CharReader): CharReader = action(env, r)

  def run(env: Env, pos: CharReader, r: CharReader): CharReader = r.error("not allowed here")

case class NumberWord(name: String, n: Double) extends SimpleWord:
  def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    env push n
    r

case class TickWord(word: Word) extends SimpleWord:
  val name = s"' ${word.name}"

  def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    env push word
    r

case class PrintWord(s: String) extends SimpleWord:
  val name = s"""." $s""""

  override def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    print(s)
    r

case class NoopWord(name: String) extends SimpleWord:
  override def run(env: Env, pos: CharReader, r: CharReader): CharReader = r

case class FalseBranchWord(name: String, idx: Int) extends SimpleWord:
  override def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    val v = env.pop

    if v == false || v == 0 || v == null || v == "" then env.pc = idx
    r

case class BranchWord(name: String, idx: Int) extends SimpleWord:
  override def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    env.pc = idx
    r

case object DoWord extends SimpleWord:
  val name = "DO"

  override def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    env.returnStack push Return.Loop(env.popn, env.popn)
    r

case class LoopWord(name: String, idx: Int, offset: Env => Double) extends SimpleWord:
  override def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    val loop @ Return.Loop(index, end) = env.returnStack.top: @unchecked
    val disp = offset(env)
    val newIndex = index + disp

    if disp > 0 && newIndex < end || disp < 0 && newIndex >= end then
      env.pc = idx
      loop.index = newIndex
    else env.returnStack.pop

    r

case class ConstantWord(name: String, value: Any) extends SimpleWord:
  override def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    env push value
    r

case class VariableWord(name: String) extends SimpleWord with Address:
  var value: Any = null

  override def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    env push this
    r

  override def toString: String = s"<address (variable '$name')>"

case class ArrayWord(name: String) extends SimpleWord with ArrayAddress:
  val array = new ArrayBuffer[Any]

  override def value: Any = array.head

  override def value_=(x: Any): Unit = array(0) = x

  override def run(env: Env, pos: CharReader, r: CharReader): CharReader =
    env push this
    r

  override def toString: String = s"<address (array '$name')>"
