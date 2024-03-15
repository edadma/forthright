package io.github.edadma.forthright

@main def run(): Unit =
  val env = new Env

  env.push(1, 2)
  println("Hello world")
