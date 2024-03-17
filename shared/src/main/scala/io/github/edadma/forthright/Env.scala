package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader
import io.github.edadma.forthright.Return.Pointer

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized
import scala.language.postfixOps
import Console.*

enum Mode:
  case Run, Compile

enum Return:
  case Pointer(caller: ArraySeq[Word], idx: Int, word: String)
  case Done

case class ConditionalBackpatch(idx: Int)
case class Backpatch(idx: Int)

class Env:
  val dataStack = new mutable.Stack[Any]
  val returnStack = new mutable.Stack[Return]
  val dictionary = new mutable.LinkedHashMap[String, Word]
  var buf = new ArrayBuffer[Word]
  var word: String = uninitialized
  var mode: Mode = Mode.Run
  var pos: CharReader = uninitialized
  var pc: Int = 0
  var code: ArraySeq[Word] = uninitialized
  var trace = false

  def debug(msg: String): Unit =
    if trace then
      println(s"$GREEN$msg")
      println(s"${dataStack map display reverse}$RESET")

  def call(definition: ArraySeq[Word]): Unit =
    returnStack push Return.Done
    code = definition
    pc = 0
    execute()

  @tailrec
  final def execute(): Unit =
    if pc < code.length then
      code(pc) match
        case SimpleWrappedWord(pos, DefinedWord(name, definition)) =>
          debug(pos.longErrorText(s">> calling $name").trim)
          returnStack push Pointer(code, pc + 1, word)
          word = name
          code = definition
          pc = 0
        case w =>
          w.run(this, null, null)
          pc += 1

      execute()
    else
      returnStack.pop match
        case Return.Pointer(caller, idx, name) =>
          debug(s"<< returning to $name from $word")
          word = name
          code = caller
          pc = idx
          execute()
        case Return.Done => debug(s"<< returning from $word")
        case _           => error("return stack cluttered")
  end execute

  def error(msg: String): Nothing =
    if pos eq null then sys.error(msg)
    else pos.error(msg)

  def pop: Any =
    if dataStack.isEmpty then error("empty data stack")
    else dataStack.pop

  def popn: Double = pop.asInstanceOf[Double]

  def popb: Boolean = pop.asInstanceOf[Boolean]

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
    dictionary(word) = DefinedWord(word, buf to ArraySeq)

  def addToDefinition(w: Word): Unit = buf += w

  def lookup(s: String, pos: CharReader): Word = dictionary.getOrElse(s.toUpperCase, pos.error("word not found"))

  def interpret(input: String): Unit = interpret(CharReader.fromString(input))

  @tailrec
  final def interpret(input: CharReader): Unit =
    consumeWord(input) match
      case Left(_) =>
      case Right((r, r1, s)) =>
        val w =
          if s.forall("-0123456789.eE" contains _) && s.exists(_.isDigit) then NumberWord(s)
          else lookup(s, r)
        val r2 =
          mode match
            case Mode.Run =>
              pos = r

              val r2 = w.run(this, r, r1)

              pos = null
              r2
            case Mode.Compile => w.compile(this, r, r1)

        interpret(skipWhitespace(r2))
  end interpret

  addToDictionary(builtin)

  interpret("""
      |: 0< 0 < ;
      |: 0= 0 = ;
      |: 0> 0 > ;
      |: 1+ 1 + ;
      |: 1- 1 - ;
      |: 2+ 2 + ;
      |: 2- 2 - ;
      |""".stripMargin)
