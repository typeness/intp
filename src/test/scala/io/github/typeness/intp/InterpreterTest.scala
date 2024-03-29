package io.github.typeness.intp

import org.scalatest.FunSuite

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class InterpreterTest extends FunSuite {
  test("Calculate arithmetic expression") {
    val parser = new Parser("2+3")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == IntegerType(2 + 3))
  }
  test("Allow multiple digits in input") {
    val parser = new Parser("12+14")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == IntegerType(12 + 14))
  }
  test("Handle input with whitespace characters") {
    val parser = new Parser("10 + 1")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == IntegerType(10 + 1))
  }
  test("Evaluate subtractions") {
    val parser = new Parser("7 - 5")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == IntegerType(7 - 5))
  }
  test("Handle multiplication") {
    val parser = new Parser("3*8")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == IntegerType(3 * 8))
  }
  test("Handle division") {
    val parser = new Parser("8/4")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == IntegerType(8 / 4))
  }
  test("Evaluate arbitrary number of additions and subtractions") {
    val parser = new Parser("9 - 5 + 3 + 11")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == IntegerType(9 - 5 + 3 + 11))
  }
  test("Evaluate single number") {
    val parser = new Parser("12345")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == IntegerType(12345))
  }
  test("Evaluate arbitrary expression") {
    val parser = new Parser("10-2*3/4+5-1")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == IntegerType(10 - 2 * 3 / 4 + 5 - 1))
  }
  test("Handle order of subtraction") {
    val parser = new Parser("7 - 3 - 1")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == IntegerType(7 - 3 - 1))
  }
  test("Handle parenthesis") {
    val parser =
      new Parser("7 + 3 * (10 / (12 / (3 + 1) - 1)) / (2 + 3) - 5 - 3 + (8)")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(
      interpreter
        .visit(ast) == IntegerType(7 + 3 * (10 / (12 / (3 + 1) - 1)) / (2 + 3) - 5 - 3 + 8)
    )
  }
  test("Handle unary minus") {
    val parser = new Parser("--2")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == IntegerType(-(-2)))
  }
  test("Evaluate arbitrary number of unary operators") {
    val parser = new Parser("5 - - - + - (3 + 4) - +2")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == IntegerType(10))
  }
  test("Numeric variable assign and read") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/variables.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "x" -> IntegerType(2),
        "y" -> IntegerType(8),
        "z" -> IntegerType(10)
      )
    )
  }
  test("Boolean expression") {
    val parser = new Parser("true or false and not true")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(interpreter.visit(ast) == BooleanType(true))
  }
  test("If statement") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/if.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map("is" -> BooleanType(true))
    )
  }
  test("If-else statements") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/if-else.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map("x" -> IntegerType(2))
    )
  }
  test("While statement") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/while.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map("x" -> IntegerType(1000000))
    )
  }
  test("Assignment of array literal") {
    val interpreter = new Interpreter()
    interpreter.runFromString("var x = [1, 2, 3]")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "x" -> ArrayType(mutable.ArrayBuffer(IntegerType(1), IntegerType(2), IntegerType(3)))
      )
    )
  }
  test("Assignment of multi-dimension array literal") {
    val interpreter = new Interpreter()
    interpreter.runFromString("var x = [[1, 2], [3, 4], [5]]")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "x" -> ArrayType(
          mutable.ArrayBuffer(
            ArrayType(mutable.ArrayBuffer(IntegerType(1), IntegerType(2))),
            ArrayType(mutable.ArrayBuffer(IntegerType(3), IntegerType(4))),
            ArrayType(mutable.ArrayBuffer(IntegerType(5)))
          )
        )
      )
    )
  }
  test("Concat of array literal") {
    val interpreter = new Interpreter()
    interpreter.runFromString("var x = [1, 2, 3] + [4, 5, 6]")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "x" -> ArrayType(
          mutable.ArrayBuffer(IntegerType(1), IntegerType(2), IntegerType(3))
            ++ mutable.ArrayBuffer(IntegerType(4), IntegerType(5), IntegerType(6))
        )
      )
    )
  }
  test("Push 10 elements to array in loop") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/array-loop.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "arr" -> ArrayType(mutable.ArrayBuffer.range(0, 10).map(IntegerType)),
        "x" -> IntegerType(10)
      )
    )
  }
  test("Array access") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/array-access.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "arr" -> ArrayType(
          mutable.ArrayBuffer(
            IntegerType(-100),
            IntegerType(33),
            IntegerType(55),
            IntegerType(56),
            IntegerType(234)
          )
        ),
        "middle" -> IntegerType(55)
      )
    )
  }
  test("Character assignment") {
    val interpreter = new Interpreter()
    interpreter.runFromString("var x = 'c'")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map("x" -> CharType('c'))
    )
  }
  test("Define a function") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/functionDefinition.intp")
  }
  test("Return function from function and call it") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/return-function.intp")
  }
  test("Call function with another function as argument") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/function-as-argument.intp")
  }
  test("Assignment for array indexed object") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/assignment-array-indexed.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "x" -> ArrayType(mutable.ArrayBuffer(1, 2, 10).map(IntegerType))
      )
    )
  }
  test("Strings manipulation") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/strings-manipulation.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "z" -> ArrayType(
          mutable
            .ArrayBuffer('A', 'l', 'a', ' ', 'm', 'a', ' ', 'k', 'o', 't', 'a', '.')
            .map(CharType)
        ),
        "y" -> ArrayType(mutable.ArrayBuffer(' ', 'm', 'a').map(CharType)),
        "x" -> ArrayType(mutable.ArrayBuffer('A', 'l', 'a').map(CharType)),
        "c" -> CharType('A')
      )
    )
  }
  test("Operator == for all possible types") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/equals.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "e" -> BooleanType(false),
        "j" -> BooleanType(false),
        "f" -> BooleanType(true),
        "a" -> BooleanType(true),
        "i" -> BooleanType(true),
        "b" -> BooleanType(false),
        "g" -> BooleanType(true),
        "c" ->
          BooleanType(true),
        "h" -> BooleanType(true),
        "d" -> BooleanType(false)
      )
    )
  }
  test("Compute recursion factorial") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/factorial.intp")
    assert(interpreter.memory.get("result").map(_.value).contains(IntegerType(120)))
  }
  test("Operator != for all possible types") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/not-equals.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "e" -> BooleanType(true),
        "j" -> BooleanType(true),
        "f" -> BooleanType(false),
        "a" -> BooleanType(false),
        "i" -> BooleanType(false),
        "b" -> BooleanType(true),
        "g" -> BooleanType(false),
        "c" ->
          BooleanType(false),
        "h" -> BooleanType(false),
        "d" -> BooleanType(true)
      )
    )
  }
  test("Run empty program") {
    val interpreter = new Interpreter()
    interpreter.runFromString("")
    assert(
      interpreter.memory.getAll == Map.empty
    )
  }
  test("Calculate with modulo operator") {
    val parser = new Parser("10 % 3")()
    val ast = parser.parse().children.head
    val interpreter = new Interpreter()
    assert(
      interpreter.visit(ast) == DoubleType(10 % 3)
    )
  }
  test("Allow reuse of functions literal") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/f-literal-reuse.intp")
  }
  test("Expression if-then-else return value") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/if-then-else.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "x" -> IntegerType(10),
        "y" -> IntegerType(12)
      )
    )
  }
  test("Builtin println function") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/println.intp")
  }
  test("Builtin size function") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/size.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "z" -> IntegerType(0),
        "y" -> IntegerType(1),
        "x" -> IntegerType(3)
      )
    )
  }
  test("Builtin print function") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/print.intp")
  }
  test("Builtin string function") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/string.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) ==
        Map(
          "one" -> ArrayType(ArrayBuffer('1').map(CharType)),
          "eleven" -> ArrayType(ArrayBuffer('1', '1').map(CharType)),
          "arr" -> ArrayType(
            ArrayBuffer('[', '0', ',', ' ', '1', '0', ',', ' ', '4', ']').map(CharType)
          ),
          "t" -> ArrayType(ArrayBuffer('t', 'r', 'u', 'e').map(CharType)),
          "f" -> ArrayType(ArrayBuffer('f', 'a', 'l', 's', 'e').map(CharType)),
          "db" -> ArrayType(ArrayBuffer('-', '1', '2', '.', '3', '4').map(CharType)),
          "test" -> ArrayType(ArrayBuffer('1', '2').map(CharType)),
          "dat" -> ArrayType(
            ArrayBuffer(
              '{', 'a', ' ', '=', ' ', '1', ',', ' ', 'c', ' ', '=', ' ', 't', 'r', 'u', 'e', '}'
            ).map(CharType)
          ),
          "str" -> ArrayType(ArrayBuffer('l', 'o', 'l').map(CharType)),
          "function" -> ArrayType(ArrayBuffer('f', 'u', 'n', 'c').map(CharType)),
          "arrFunc" -> ArrayType(ArrayBuffer('[', 'f', 'u', 'n', 'c', ']').map(CharType))
        )
    )
  }
  test("Builtin assert function") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/assert.intp")
  }
  test("FizzBuzz function") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/fizzBuzz.intp")
  }
  test("Return instruction break while loop") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/whileReturn.intp")
  }
  test("Using object literal") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/objectLiteral.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) ==
        Map(
          "person" -> ObjectType(
            mutable.Map(
              "surname" -> ArrayType(ArrayBuffer('S', 'm', 'i', 't', 'h').map(CharType)),
              "name" -> ArrayType(ArrayBuffer('J', 'o', 'h', 'n').map(CharType))
            )
          )
        )
    )
  }
  test("Object literal property access") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/propertyAccess.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "f" -> IntegerType(2),
        "obj" -> ObjectType(
          mutable.Map("x" -> IntegerType(1), "z" -> IntegerType(3), "y" -> IntegerType(2))
        )
      )
    )
  }
  test("Object literal as property") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/objectLiteralAsProperty.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) ==
        Map(
          "f" -> CharType('y'),
          "obj" -> ObjectType(
            mutable
              .Map("x" -> ObjectType(mutable.Map("g" -> CharType('y'))), "y" -> IntegerType(-4))
          )
        )
    )
  }
  test("Data keyword as syntax sugar over function returning object literal") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/data.intp")
    assert(
      interpreter.memory
        .get("n")
        .map(_.value)
        .contains(ArrayType(ArrayBuffer('J', 'o', 'h', 'n').map(CharType)))
    )
    assert(
      interpreter.memory
        .get("s")
        .map(_.value)
        .contains(ArrayType(ArrayBuffer('S', 'm', 'i', 't', 'h').map(CharType)))
    )
    assert(
      interpreter.memory
        .get("n2")
        .map(_.value)
        .contains(ArrayType(ArrayBuffer('A', 'm', 'y').map(CharType)))
    )
    assert(
      interpreter.memory
        .get("s2")
        .map(_.value)
        .contains(ArrayType(ArrayBuffer('N', 'e', 'w').map(CharType)))
    )
  }
  test("Valid conversions via casting functions") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/castingFunctions.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "int2int" -> IntegerType(3),
        "int2char" -> CharType('1'),
        "int2double" -> DoubleType(34.0),
        "char2int" -> IntegerType(97),
        "char2char" -> CharType('x'),
        "char2double" -> DoubleType(101.0),
        "double2int" -> IntegerType(2),
        "double2char" -> CharType('B'),
        "double2double" -> DoubleType(23.45)
      )
    )

  }
  test("Invalid conversions via casting functions") {
    val interpreter = new Interpreter()
    intercept[CastError] {
      interpreter.runFromResource("interpreter/invalidCasting.intp")
    }
  }
  test("Property access of in place defined object literal") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/accessObjectLiteralInPlace.intp")
    assert(
      interpreter.memory.getAll
        .mapValues(_.value) == Map("y" -> IntegerType(3), "z" -> BooleanType(true))
    )
  }
  test("Defining array and indexing it in one place") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/arrayIndexingInOneStep.intp")
    assert(interpreter.memory.getAll.mapValues(_.value) == Map("test" -> IntegerType(123)))
  }
  test("Defining function and calling it in one place") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/functionCallInOneStep.intp")
    assert(interpreter.memory.getAll.mapValues(_.value) == Map("p" -> IntegerType(-9)))
  }
  test("Combine definition of array, function and object with indexing, call and access") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/fluentSyntax.intp")
    assert(interpreter.memory.getAll.mapValues(_.value) == Map("test" -> DoubleType(3.14)))
  }
  test("Property assignment") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/propertyAssignment.intp")
    assert(
      interpreter.memory.getAll.mapValues(_.value) == Map(
        "test" -> ObjectType(mutable.Map("a" -> CharType('x')))
      )
    )
  }
  test("While local scope do not leak definitions") {
    val interpreter = new Interpreter()
    intercept[UndefinedVariable] {
      interpreter.runFromResource("interpreter/whileLocalScope.intp")
    }
  }
  test("If local scope do not leak defintions") {
    val interpreter = new Interpreter()
    intercept[UndefinedVariable] {
      interpreter.runFromResource("interpreter/ifLocalScope.intp")
    }
  }
  test("string function is idempotent") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/stringIdempotency.intp")
    assert(interpreter.memory.get("test").map(_.value).contains(BooleanType(true)))
  }
  test("string function on empty array literal") {
    val interpreter = new Interpreter()
    interpreter.runFromString("val a = string([])")
    assert(
      interpreter.memory
        .get("a")
        .map(_.value)
        .contains(ArrayType(mutable.ArrayBuffer('[', ']').map(CharType)))
    )
  }
  test("Access array element out of bounds") {
    val interpreter = new Interpreter()
    intercept[IndexOutOfBound] {
      interpreter.runFromResource("interpreter/indexOutOfBounds.intp")
    }
  }
  test("Object's equality operators") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/objectEquality.intp")
  }
  test("Escape special ASCII characters") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/ascii.intp")
    assert(
      interpreter.memory.get("str").map(_.value).get ==
        ArrayType("a\nb\\\"\t\r\b".map(CharType).to[mutable.ArrayBuffer])
    )
    assert(
      interpreter.memory.get("n").map(_.value).get ==
        CharType('\n')
    )
    assert(
      interpreter.memory.get("r").map(_.value).get ==
        CharType('\r')
    )
    assert(
      interpreter.memory.get("b").map(_.value).get ==
        CharType('\b')
    )
    assert(
      interpreter.memory.get("t").map(_.value).get ==
        CharType('\t')
    )
    assert(
      interpreter.memory.get("ap").map(_.value).get ==
        CharType('\'')
    )
    assert(
      interpreter.memory.get("zero").map(_.value).get ==
        CharType(0.toChar)
    )
  }
  test("Desugaring of compound assignments") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/compound.intp")
    assert(
      interpreter.memory.get("a").map(_.value).get == IntegerType(3)
    )
    assert(
      interpreter.memory.get("b").map(_.value).get == IntegerType(-3)
    )
    assert(
      interpreter.memory.get("c").map(_.value).get == IntegerType(16)
    )
    assert(
      interpreter.memory.get("d").map(_.value).get == DoubleType(1.0)
    )
    assert(
      interpreter.memory.get("e").map(_.value).get == IntegerType(3)
    )
    assert(
      interpreter.memory.get("arr").map(_.value).get == ArrayType(
        List(1, 2, 3, 4, 5).map(IntegerType).to[mutable.ArrayBuffer]
      )
    )
  }

  test("For loop") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/forLoop.intp")
  }
  test("Closures") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/closures.intp")
  }
  test("Break statement") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/break.intp")
  }
  test("Scopes") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/scopes.intp")
  }
  test("Variables definition inside loops") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/loopDefs.intp")
  }
  test("Short-circuit evaluation") {
    val interpreter = new Interpreter()
    interpreter.runFromResource("interpreter/shortCircuitEval.intp")
  }
}
