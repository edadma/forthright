package io.github.edadma.forthright

def display(a: Any): String =
  a match
    case n: Double => if n.isWhole then n.toInt.toString else n.toString
    case _         => a.toString
