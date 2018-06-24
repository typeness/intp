package io.github.typeness.intp

import org.scalatest.FunSuite

import scala.collection.mutable

class MemoryTest extends FunSuite {
  test("Define and read variables from memory") {
    val memory = new Memory()
    memory.define("a" -> IntegerType(2))
    memory.define("b" -> BooleanType(true))
    memory.define("c" -> ArrayType(mutable.ArrayBuffer(DoubleType(12.3), CharType('a'))))
    assert(memory.get("a").contains(IntegerType(2)))
    assert(memory.get("b").contains(BooleanType(true)))
    assert(memory.get("c").contains(ArrayType(mutable.ArrayBuffer(DoubleType(12.3), CharType('a')))))
  }
  test("Global variables and closures") {
    val memory = new Memory()
    memory.define("global" -> BooleanType(true))
    // call some function
    memory.pushNewStack()
    memory.define("localInFunction" -> DoubleType(0.1))
    // call inner function from inside outer
    memory.pushNewStack()
    memory.define("localInNestedFunction" -> IntegerType(44))

    assert(memory.get("global").contains(BooleanType(true)))
    assert(memory.get("localInNestedFunction").contains(IntegerType(44)))
    // from closure
    assert(memory.get("localInFunction").contains(DoubleType(0.1)))

    memory.popStack()
    assert(memory.get("global").contains(BooleanType(true)))
    // after pop stack localInNestedFunction in erased
    assert(memory.get("localInNestedFunction").isEmpty)
    // but localInFunction is still here
    assert(memory.get("localInFunction").contains(DoubleType(0.1)))

    // return to global scope
    memory.popStack()
    assert(memory.get("global").contains(BooleanType(true)))
    assert(memory.get("localInNestedFunction").isEmpty)
    assert(memory.get("localInFunction").isEmpty)
  }
  test("Overriding global variables and changing them") {
    val memory = new Memory()
    memory.define("var" -> BooleanType(true))
    memory.pushNewStack()
    memory.define("var" -> BooleanType(false))
    assert(memory.get("var").contains(BooleanType(false)))
    memory.popStack()
    assert(memory.get("var").contains(BooleanType(true)))
  }
}
