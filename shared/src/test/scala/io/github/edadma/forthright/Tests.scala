package io.github.edadma.forthright

import scala.collection.mutable

trait Tests:
  def stack(test: String): mutable.Stack[Any] =
    val env = new Env

    interpret(env, ": n1 3 ; : n2 4 ; : n3 5 ;")
    interpret(env, test)
    env.dataStack
