package io.github.typeness.intp

import org.scalatest.FunSuite

import scala.collection.mutable

class MemoryTest extends FunSuite {
  test("Define and read variables from memory") {
    val memory = new Memory()
    memory.define("a" -> IntegerType(2), isMutable = true)
    memory.define("b" -> BooleanType(true), isMutable = true)
    memory.define(
      "c" -> ArrayType(mutable.ArrayBuffer(DoubleType(12.3), CharType('a'))),
      isMutable = true
    )
    assert(memory.get("a").map(_.value).contains(IntegerType(2)))
    assert(memory.get("b").map(_.value).contains(BooleanType(true)))
    assert(
      memory
        .get("c")
        .map(_.value)
        .contains(ArrayType(mutable.ArrayBuffer(DoubleType(12.3), CharType('a'))))
    )
  }
  test("Global variables and closures") {
    val memory = new Memory()
    memory.define("global" -> BooleanType(true), isMutable = true)
    // call some function
    memory.pushNewStack()
    memory.define("localInFunction" -> DoubleType(0.1), isMutable = true)
    // call inner function from inside outer
    memory.pushNewStack()
    memory.define("localInNestedFunction" -> IntegerType(44), isMutable = true)

    assert(memory.get("global").map(_.value).contains(BooleanType(true)))
    assert(memory.get("localInNestedFunction").map(_.value).contains(IntegerType(44)))
    // from closure
    assert(memory.get("localInFunction").map(_.value).contains(DoubleType(0.1)))

    memory.popStack()
    assert(memory.get("global").map(_.value).contains(BooleanType(true)))
    // after pop stack localInNestedFunction in erased
    assert(memory.get("localInNestedFunction").isEmpty)
    // but localInFunction is still here
    assert(memory.get("localInFunction").map(_.value).contains(DoubleType(0.1)))

    // return to global scope
    memory.popStack()
    assert(memory.get("global").map(_.value).contains(BooleanType(true)))
    assert(memory.get("localInNestedFunction").isEmpty)
    assert(memory.get("localInFunction").isEmpty)
  }
  test("Overriding global variables and changing them") {
    val memory = new Memory()
    memory.define("variable" -> BooleanType(true), isMutable = true)
    memory.pushNewStack()
    memory.define("variable" -> BooleanType(false), isMutable = true)
    assert(memory.get("variable").map(_.value).contains(BooleanType(false)))
    memory.popStack()
    assert(memory.get("variable").map(_.value).contains(BooleanType(true)))
  }
}
