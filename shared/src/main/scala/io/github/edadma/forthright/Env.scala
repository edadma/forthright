package io.github.edadma.forthright

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

  def pop: Any =
    if dataStack.isEmpty then sys.error("empty data stack")
    else dataStack.pop

  def popi: Int = dataStack.pop.asInstanceOf[Int]

  infix def push(v: Any): Unit = dataStack push v

  def push(a: Any, b: Any, c: Any*): Unit = dataStack.push(a, b, c*)

  def pushAll(elems: scala.collection.IterableOnce[Any]): Unit = dataStack.pushAll(elems)

  infix def npop(n: Int): Seq[Any] = (1 to n) map (_ => pop) reverse

  infix def npopn(n: Int): Seq[Int] = ((1 to n) map (_ => pop) reverse).asInstanceOf[Seq[Int]]

  def addToDictionary(words: Seq[Word]): Unit = dictionary ++= words.map(w => w.name -> w)

  def openDefinition(name: String): Unit =
    word = name
    buf.clear
    mode = Mode.Compile

  def closeDefinition(): Unit =
    mode = Mode.Run
    dictionary(word) = Definition(word, buf to ArraySeq)

  def addToDefinition(w: Word): Unit = buf += w

  addToDictionary(builtin)
