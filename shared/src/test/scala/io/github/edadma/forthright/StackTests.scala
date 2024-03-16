package io.github.edadma.forthright

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StackTests extends AnyFreeSpec with Matchers with Tests {

  "dup" in { stack("n1 dup") shouldBe stack("n1 n1") }
  "?dup yes" in { stack("3 ?dup") shouldBe stack("n1 n1") }
  "?dup no" in { stack("0 ?dup") shouldBe stack("0") }
  "drop" in { stack("n1 drop") shouldBe stack("") }
  "rot" in { stack("n1 n2 n3 rot") shouldBe stack("n2 n3 n1") }
  "over" in { stack("n1 n2 over") shouldBe stack("n1 n2 n1") }
  "swap" in { stack("n1 n2 swap") shouldBe stack("n2 n1") }
  "pick" in { stack("n1 n2 n3 3 pick") shouldBe stack("n1 n2 n3 n1") }
  "roll" in { stack("n1 n2 n3 3 roll") shouldBe stack("n2 n3 n1") }

  "+" in { stack("3 4 +") shouldBe stack("7") }
  "-" in { stack("3 4 -") shouldBe stack("-1") }
  "*" in { stack("3 4 *") shouldBe stack("12") }
  "* decimals" in { stack("3 .5 *") shouldBe stack("1.5") }
  "/" in { stack("12 4 /") shouldBe stack("3") }

}
