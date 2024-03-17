package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader
import io.github.edadma.forthright.Return.Pointer

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

enum Mode:
  case Run, Compile

enum Return:
  case Pointer(caller: ArraySeq[Word], idx: Int)
  case Done

class Env:
  val dataStack = new mutable.Stack[Any]
  val returnStack = new mutable.Stack[Return]
  val dictionary = new mutable.LinkedHashMap[String, Word]
  var buf = new ListBuffer[Word]
  var word: String = null
  var mode: Mode = Mode.Run
  var pos: CharReader = null
  var pc: Int = 0
  var code: ArraySeq[Word] = null

  def call(definition: ArraySeq[Word]): Unit =
    code = definition
    pc = 0
    execute()

  @tailrec
  final def execute(): Unit =
    if pc < code.length then
      code(pc) match
        case Definition(_, definition) =>
          returnStack push Pointer(code, pc + 1)
          code = definition
          pc = 0
        case w =>
          w.run(this, null)
          pc += 1

      execute()
    else
      returnStack.pop match
        case Return.Pointer(caller, idx) =>
          code = caller
          pc = idx
          execute()
        case Return.Done =>
        case _           => error("return stack cluttered")

  def error(msg: String): Nothing =
    if pos eq null then sys.error(msg)
    else pos.error(msg)

  def pop: Any =
    if dataStack.isEmpty then error("empty data stack")
    else dataStack.pop

  def popn: Double = pop.asInstanceOf[Double]

  def popi: Int =
    popn match
      case n if n.isWhole => n.toInt
      case _              => error("integer expected")

  def popp: Int =
    popi match
      case n if n > 0 => n
      case _          => error("postive integer expected")

  infix def push(v: Any): Unit = dataStack push v

  def push(a: Any, b: Any, c: Any*): Unit = dataStack.push(a, b, c*)

  infix def pushn(n: Number): Unit = push(n.doubleValue)

  def pushAll(elems: scala.collection.IterableOnce[Any]): Unit = dataStack.pushAll(elems)

  infix def npop[T](n: Int): Seq[T] = ((1 to n) map (_ => pop)).asInstanceOf[Seq[T]] reverse

  infix def npopn(n: Int): Seq[Double] = npop[Double](n)

  infix def exec2[T](action: (T, T) => Any): Any =
    val Seq(a, b) = npop[T](2)

    action(a, b)

  infix def execn2(action: (Double, Double) => Any): Any = exec2[Double](action)

  def addToDictionary(words: Seq[Word]): Unit =
    val l = words.map(w => w.name -> w)

    l.groupBy(_._1).find(_._2.length > 1) match
      case Some((name, _)) => sys.error(s"duplicate word '$name'")
      case None            => dictionary ++= l

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
