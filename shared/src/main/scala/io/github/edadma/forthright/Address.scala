package io.github.edadma.forthright

import io.github.edadma.char_reader.CharReader

import scala.collection.mutable.ArrayBuffer

trait Address:
  def value: Any
  def value_=(x: Any): Unit

trait ArrayAddress extends Address:
  def array: ArrayBuffer[Any]
  def value: Any
  def value_=(x: Any): Unit

  def index(offset: Int, pos: CharReader): IndexedArrayAddress =
    def check(idx: Int): Unit = if idx < 0 then pos.error("an array can't have a negative index")

    this match
      case w: ArrayWord =>
        check(offset)
        IndexedArrayAddress(w.array, offset)
      case IndexedArrayAddress(array, idx) =>
        check(idx + offset)
        IndexedArrayAddress(array, idx + offset)

case class IndexedArrayAddress(array: ArrayBuffer[Any], idx: Int) extends ArrayAddress:
  def value: Any = array(idx)
  def value_=(x: Any): Unit = array(idx) = x
