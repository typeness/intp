package io.github.typeness.intp

import org.scalatest.FunSuite

class MemoryTest extends FunSuite {
  test("Define and read variables from memory") {
    val memory = new Memory()
    memory.define("a" -> 2)
    memory.define("b" -> true)
    memory.define("c" -> Vector(12.3, 'a'))
    assert(memory.get("a").contains(2))
    assert(memory.get("b").contains(true))
    assert(memory.get("c").contains(Vector(12.3, 'a')))
  }
  test("Global variables and closures") {
    val memory = new Memory()
    memory.define("global" -> true)
    // call some function
    memory.pushNewStack()
    memory.define("localInFunction" -> 0.1)
    // call inner function from inside outer
    memory.pushNewStack()
    memory.define("localInNestedFunction" -> 44)

    assert(memory.get("global").contains(true))
    assert(memory.get("localInNestedFunction").contains(44))
    // from closure
    assert(memory.get("localInFunction").contains(0.1))

    memory.popStack()
    assert(memory.get("global").contains(true))
    // after pop stack localInNestedFunction in erased
    assert(memory.get("localInNestedFunction").isEmpty)
    // but localInFunction is still here
    assert(memory.get("localInFunction").contains(0.1))

    // return to global scope
    memory.popStack()
    assert(memory.get("global").contains(true))
    assert(memory.get("localInNestedFunction").isEmpty)
    assert(memory.get("localInFunction").isEmpty)
  }
}
