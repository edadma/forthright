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
  case Loop(var index: Double, var end: Double)
  case Data(value: Any)

case class If(idx: Int)
case class Else(idx: Int)
case class Do(idx: Int)
case class Begin(idx: Int)
case class While(begin: Int, idx: Int)

class Env extends Address:
  var base: Double = 10
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

  def value: Any = base
  def value_=(x: Any): Unit = base = x.asInstanceOf[Double]

  def stack: String = (dataStack map display).reverse mkString " "

  def debug(msg: => String): Unit =
    if trace then
      println(s"$GREEN$msg")
      println(s"stack: $stack$RESET")

  def call(definition: ArraySeq[Word]): Unit =
    returnStack push Return.Done
    code = definition
    pc = 0
    execute()

  @tailrec
  final def execute(): Unit =
    if pc < code.length then
      code(pc) match
        case word if word.name.startsWith("( ") || word.name == "ELSE" || word.name == "THEN" || word.name == "LOOP" =>
          word.run(this, pos, null)
          pc += 1
        case WrapperWord(pos, DefinedWord(name, definition)) =>
          debug(pos.longErrorText(s">> $name").trim)
          returnStack push Pointer(code, pc + 1, word)
          word = name
          code = definition
          pc = 0
        case WrapperWord(pos, word) =>
          debug(pos.longErrorText(word.name).trim)
          word.run(this, pos, null)
          pc += 1
        case word =>
          debug(word.name)
          word.run(this, pos, null)
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

  infix def npop[T](n: Int): Vector[T] = ((1 to n) map (_ => pop)).asInstanceOf[Vector[T]] reverse

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

  def defined(name: String): Boolean = dictionary.contains(name.toUpperCase)

  def display(a: Any): String =
    a match
      case n: Double => if n.isWhole then Integer.toString(n.toInt, base.toInt) else n.toString
      case _         => String.valueOf(a)

  def interpret(input: String): Unit = interpret(CharReader.fromString(input))

  @tailrec
  final def interpret(input: CharReader): Unit =
    consumeWord(input) match
      case Left(_) =>
      case Right((r, r1, s)) =>
        val w =
          if s.forall("-0123456789.abcdefABCDEF" contains _) && s.exists(_.isDigit) && !defined(s) then
            NumberWord(s, if base == 10 then s.toDouble else Integer.parseInt(s, base.toInt))
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
      |CREATE PAD 1000 ALLOT
      |: 0< ( n -- flag ) 0 < ;
      |: 0= ( n -- flag ) 0 = ;
      |: 0> ( n -- flag ) 0 > ;
      |: 1+ ( n -- n+1 ) 1 + ;
      |: 1- ( n -- n-1 ) 1 - ;
      |: 2+ ( n -- n+2 ) 2 + ;
      |: 2- ( n -- n-2 ) 2 - ;
      |: 2DUP ( n1 n2 -- n1 n2 n1 n2 ) OVER OVER ;
      |: 2DROP ( n1 n2 -- ) DROP DROP ;
      |: MIN ( n1 n2 -- n ) 2DUP < IF DROP ELSE SWAP DROP THEN ;
      |: MAX ( n1 n2 -- n ) 2DUP > IF DROP ELSE SWAP DROP THEN ;
      |: SPACES ( n -- ) 0 DO SPACE LOOP ;
      |: +! ( n addr -- ) DUP @ ROT + SWAP ! ;
      |: 1+! ( addr -- ) DUP @ 1 + SWAP ! ;
      |: 1-! ( addr -- ) DUP @ -1 + SWAP ! ;
      |""".stripMargin)
