package io.github.edadma.forthright

import org.jline.reader.EndOfFileException
import org.jline.reader.LineReaderBuilder
import org.jline.reader.UserInterruptException
import org.jline.reader.impl.DefaultParser
import org.jline.terminal.TerminalBuilder

@main def run(): Unit =
  val env = new Env
  val terminal = TerminalBuilder.builder().build()
  val parser = new DefaultParser()
  val reader = LineReaderBuilder
    .builder()
    .terminal(terminal)
    .parser(parser)
    .build()

  println("Forthright v0.0.1")
  println
  println("Type Ctrl-D to exit.")

  while true do
    try {
      val line = reader.readLine("> ")

      env.interpret(line)
      println
    } catch {
      case _: UserInterruptException => // Ignore
      case _: EndOfFileException     => return
      case e: Exception =>
        e.printStackTrace()
        println
    }
