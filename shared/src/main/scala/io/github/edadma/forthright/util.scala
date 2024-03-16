package io.github.edadma.forthright

def number(n: Double): String = if n.isWhole then n.toInt.toString else n.toString
