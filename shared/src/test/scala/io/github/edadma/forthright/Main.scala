package io.github.edadma.forthright

import pprint.pprintln

@main def run(): Unit =
  val env = new Env

  env.push(1, 2, 3)

  println(env.dataStack(0))
  println(env.pop)
