package io.github.edadma.forthright

import scala.collection.mutable

trait Tests:
  def stack(test: String): mutable.Stack[Any] =
    val env = new Env

    env.interpret(": n 21234 ; : n1 31234 ; : n2 41234 ; : n3 51234 ;")
    env.interpret(test)
    env.dataStack
