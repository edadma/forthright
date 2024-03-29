package io.github.edadma.forthright

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StackTests extends AnyFreeSpec with Matchers with Tests:
  "dup" in { stack("n1 dup") shouldBe stack("n1 n1") }
  "?dup yes" in { stack("n ?dup") shouldBe stack("n n") }
  "?dup no" in { stack("0 ?dup") shouldBe stack("0") }
  "drop" in { stack("n1 drop") shouldBe stack("") }
  "rot" in { stack("n1 n2 n3 rot") shouldBe stack("n2 n3 n1") }
  "over" in { stack("n1 n2 over") shouldBe stack("n1 n2 n1") }
  "swap" in { stack("n1 n2 swap") shouldBe stack("n2 n1") }
  "pick" in { stack("n1 n2 n3 3 pick") shouldBe stack("n1 n2 n3 n1") }
  "roll" in { stack("n1 n2 n3 3 roll") shouldBe stack("n2 n3 n1") }
  "depth" in { stack("n1 n2 n3 depth") shouldBe stack("n1 n2 n3 3") }

  "+" in { stack("3 4 +") shouldBe stack("7") }
  "-" in { stack("3 4 -") shouldBe stack("-1") }
  "*" in { stack("3 4 *") shouldBe stack("12") }
  "* decimals" in { stack("3 .5 *") shouldBe stack("1.5") }
  "/" in { stack("12 4 /") shouldBe stack("3") }
  "mod 1" in { stack("12 4 mod") shouldBe stack("0") }
  "mod 2" in { stack("12 5 mod") shouldBe stack("2") }
  "mod 3" in { stack("1.5 .5 mod") shouldBe stack("0") }
  "negate" in { stack("3 negate") shouldBe stack("-3") }

  "<" in { stack("3 4 <") shouldBe stack("true") }
  ">" in { stack("3 4 >") shouldBe stack("false") }
  "=" in { stack("3 4 =") shouldBe stack("false") }
  "<>" in { stack("3 4 <>") shouldBe stack("true") }

  "or, boolean 1" in { stack("true false or") shouldBe stack("true") }
  "or, boolean 2" in { stack("false false or") shouldBe stack("false") }
  "or, boolean 3" in { stack("true true or") shouldBe stack("true") }
  "or, boolean 4" in { stack("false true or") shouldBe stack("true") }
  "or, numeric" in { stack("5 3 or") shouldBe stack("7") }

  "and, boolean 1" in { stack("true false and") shouldBe stack("false") }
  "and, boolean 2" in { stack("false false and") shouldBe stack("false") }
  "and, boolean 3" in { stack("true true and") shouldBe stack("true") }
  "and, boolean 4" in { stack("false true and") shouldBe stack("false") }
  "and, numeric" in { stack("5 3 and") shouldBe stack("1") }

  "xor, boolean 1" in { stack("true false xor") shouldBe stack("true") }
  "xor, boolean 2" in { stack("false false xor") shouldBe stack("false") }
  "xor, boolean 3" in { stack("true true xor") shouldBe stack("false") }
  "xor, boolean 4" in { stack("false true xor") shouldBe stack("true") }
  "xor, numeric" in { stack("5 3 xor") shouldBe stack("6") }

  "not, boolean 1" in { stack("true not") shouldBe stack("false") }
  "not, boolean 2" in { stack("false not") shouldBe stack("true") }

  "if 1" in { stack(": a true if 3 then 4 ; a") shouldBe stack("3 4") }
  "if 2" in { stack(": a false if 3 then 4 ; a") shouldBe stack("4") }
  "if 3" in { stack(": a true if 3 else 4 then 5 ; a") shouldBe stack("3 5") }
  "if 4" in { stack(": a false if 3 else 4 then 5 ; a") shouldBe stack("4 5") }

  "2dup" in { stack("n1 n2 2dup") shouldBe stack("n1 n2 n1 n2") }
  "min 1" in { stack("3 4 min") shouldBe stack("3") }
  "min 2" in { stack("4 3 min") shouldBe stack("3") }
  "min 3" in { stack("3 3 min") shouldBe stack("3") }
  "max 1" in { stack("3 4 max") shouldBe stack("4") }
  "max 2" in { stack("4 3 max") shouldBe stack("4") }
  "max 3" in { stack("3 3 max") shouldBe stack("3") }
  "abs 1" in { stack("3 abs") shouldBe stack("3") }
  "abs 2" in { stack("-3 abs") shouldBe stack("3") }
  "abs 3" in { stack("0 abs") shouldBe stack("0") }

  "do...loop 1" in { stack(": a 3 0 do i loop ; a") shouldBe stack("0 1 2") }
  "do...loop 2" in { stack(": a 0 3 do i -1 +loop ; a") shouldBe stack("3 2 1 0") }
  "do...loop 3" in { stack(": a 2 0 do 12 10 do i j loop loop ; a") shouldBe stack("10 0 11 0 10 1 11 1") }

  "leave 1" in { stack(": a 3 0 do i 2 < if i else leave then loop ; a") shouldBe stack("0 1") }
  "leave 2" in { stack(": a 0 2 do i 0> if i else leave then -1 +loop ; a") shouldBe stack("2 1") }

  "begin...until" in {
    stack("variable count : a 1 count ! begin count @ dup 1+ count ! dup 3 = until ; a") shouldBe stack("1 2 3")
  }
  "begin...while...repeat" in {
    stack("variable count : a 0 count ! begin count @ 1+ dup count ! 3 <= while count @ repeat ; a") shouldBe
      stack("1 2 3")
  }
  "begin...again" in {
    stack(
      "variable count : a 0 count ! begin count @ 3 < if count @ count 1+! else exit then again ; a",
    ) shouldBe stack("0 1 2")
  }

  "base 1" in { stack("decimal 1.5") shouldBe stack("1.5") }
  "base 2" in { stack("hex 34") shouldBe stack("3 16 * 4 +") }
  "base 3" in { stack("octal 34") shouldBe stack("3 8 * 4 +") }
  "base 4" in { stack("binary 1010") shouldBe stack("10") }
  "base 5" in { stack("5 base ! base @ 34") shouldBe stack("5 3 5 * 4 +") }
