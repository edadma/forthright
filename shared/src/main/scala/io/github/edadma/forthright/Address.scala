package io.github.edadma.forthright

import scala.collection.mutable

trait Address:
  def value: Any
  def value_=(x: Any): Unit

case class ArrayAddress(array: mutable.IndexedSeq[Any], idx: Int) extends Address:
  def value: Any = array(idx)
  def value_=(x: Any): Unit = array(idx) = x
