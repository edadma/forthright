package io.github.edadma.forthright

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StackTests extends AnyFreeSpec with Matchers with Tests {

  "dup" in { stack("n1 dup") shouldBe stack("n1 n1") }
  "rot" in { stack("n1 n2 n3 rot") shouldBe stack("n2 n3 n1") }
  "over" in { stack("n1 n2 over") shouldBe stack("n1 n2 n1") }
  "swap" in { stack("n1 n2 swap") shouldBe stack("n2 n1") }

}
