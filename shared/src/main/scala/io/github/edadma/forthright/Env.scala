package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

enum Mode:
  case Run, Compile

class Env:
  val dataStack = new mutable.Stack[Any]
  val returnStack = new mutable.Stack[Any]
  val dictionary = new mutable.LinkedHashMap[String, Word]
  var buf = new ListBuffer[Word]
  var word: String = null
  var mode: Mode = Mode.Run
  var pc: Int = 0
  var pos: CharReader = null

  def error(msg: String): Nothing =
    if pos eq null then sys.error(msg)
    else pos.error(msg)

  def pop: Any =
    if dataStack.isEmpty then error("empty data stack")
    else dataStack.pop

  def popi: Int = dataStack.pop.asInstanceOf[Int]

  infix def push(v: Any): Unit = dataStack push v

  def push(a: Any, b: Any, c: Any*): Unit = dataStack.push(a, b, c*)

  def pushAll(elems: scala.collection.IterableOnce[Any]): Unit = dataStack.pushAll(elems)

  infix def npop[T](n: Int): Seq[T] = ((1 to n) map (_ => pop)).asInstanceOf[Seq[T]] reverse

  infix def npopn(n: Int): Seq[Int] = npop[Int](n)

  infix def exec2[T](action: (T, T) => Any): Any =
    val Seq(a, b) = npop[T](2)

    action(a, b)

  infix def execn2(action: (Int, Int) => Any): Any = exec2[Int](action)

  def addToDictionary(words: Seq[Word]): Unit = dictionary ++= words.map(w => w.name -> w)

  def openDefinition(name: String): Unit =
    word = name.toUpperCase
    buf.clear
    mode = Mode.Compile

  def closeDefinition(): Unit =
    mode = Mode.Run
    dictionary(word) = Definition(word, buf to ArraySeq)

  def addToDefinition(w: Word): Unit = buf += w

  def lookup(s: String, pos: CharReader): Word = dictionary.getOrElse(s.toUpperCase, pos.error("word not found"))

  addToDictionary(builtin)
