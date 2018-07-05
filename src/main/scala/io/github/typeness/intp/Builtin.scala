package io.github.typeness.intp


import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object Builtin {

  private def isString(seq: mutable.ArrayBuffer[TopType]) = {
    try {
      val _ = seq.map(_.asInstanceOf[CharType])
      true
    } catch {
      case _: Exception => false
    }
  }


  def show(arg: TopType): String = arg match {
    case ArrayType(seq: mutable.ArrayBuffer[TopType]) =>
      if (seq.nonEmpty && isString(seq)) s"${seq.mkString("")}"
      else s"[${seq.map(show).mkString(", ")}]"
    case ObjectType(map: mutable.Map[_, TopType]) =>
      "{" + map.map { case (k, v) => s"$k = ${show(v)}" }.mkString(", ") + "}"
    case FunctionType(FunctionLiteral(_, _, token)) =>
      token.value
    case value =>
      value.toString
  }

  type BuiltinFunction = (List[TopType], CompilationUnit, Position) => TopType

  object BuiltinFunction {
    def apply(fn: BuiltinFunction, argsSize: Int, arity: Int): BuiltinFunction = {
      if (argsSize != arity)
        (_: List[TopType], compilationUnit: CompilationUnit, position: Position) =>
          throw WrongFunctionCall("", argsSize, arity, compilationUnit, position)
      else
        fn
    }
  }

  private def string(args: List[TopType], cu: CompilationUnit, pos: Position): TopType =
    BuiltinFunction(
      (arg: List[TopType], _: CompilationUnit, _: Position) => {
        ArrayType(show(arg.head).map(CharType).to[mutable.ArrayBuffer])
      },
      args.length,
      1
    )(args, cu, pos)

  private def println(args: List[TopType], cu: CompilationUnit, pos: Position): TopType =
    BuiltinFunction(
      (args: List[TopType], _: CompilationUnit, _: Position) => {
        scala.Console.println(show(args.head))
        UnitType
      },
      args.size,
      1
    )(args, cu, pos)

  private def print(args: List[TopType], cu: CompilationUnit, pos: Position): TopType =
    BuiltinFunction(
      (args: List[TopType], _: CompilationUnit, _: Position) => {
        scala.Console.print(show(args.head))
        UnitType
      },
      args.size,
      1
    )(args, cu, pos)

  private def size(args: List[TopType], cu: CompilationUnit, pos: Position): TopType =
    BuiltinFunction(
      (args: List[TopType], compilationUnit: CompilationUnit, position: Position) =>
        args.head match {
          case ArrayType(seq) => IntegerType(seq.size)
          case value => throw TypeMismatch(value, "Array", compilationUnit, position)
        },
      args.size,
      1
    )(args, cu, pos)

  private def read(args: List[TopType], cu: CompilationUnit, pos: Position): TopType =
    BuiltinFunction(
      (_: List[TopType], _: CompilationUnit, _: Position) =>
        ArrayType(scala.io.StdIn.readLine().map(CharType).to[mutable.ArrayBuffer]),
      args.size,
      0
    )(args, cu, pos)

  private def assert(args: List[TopType], cu: CompilationUnit, pos: Position): TopType =
    BuiltinFunction(
      (args: List[TopType], compilationUnit: CompilationUnit, position: Position) =>
        args.head match {
          case BooleanType(bool) =>
            if (!bool) throw AssertionError(compilationUnit, position)
            else UnitType
          case value => throw TypeMismatch(value, "Boolean", compilationUnit, position)
        },
      args.size,
      1
    )(args, cu, pos)

  private def int(args: List[TopType], cu: CompilationUnit, pos: Position): TopType =
    BuiltinFunction(
      (args: List[TopType], compilationUnit: CompilationUnit, position: Position) =>
        args.head match {
          case IntegerType(v) => IntegerType(v)
          case CharType(v) => IntegerType(v.toInt)
          case DoubleType(v) => IntegerType(v.toInt)
          case ArrayType(seq) => Try(seq.mkString.toInt) match {
            case Success(x) => IntegerType(x)
            case Failure(_) => throw TypeMismatch(seq.mkString, "Integer", compilationUnit, position)
          }
          case value =>
            throw CastError(value.toString, "Integer", compilationUnit, position)
        },
      args.size,
      1
    )(args, cu, pos)

  private def char(args: List[TopType], cu: CompilationUnit, pos: Position): TopType =
    BuiltinFunction(
      (args: List[TopType], compilationUnit: CompilationUnit, position: Position) =>
        args.head match {
          case IntegerType(v) => CharType(v.toChar)
          case CharType(v) => CharType(v)
          case DoubleType(v) => CharType(v.toChar)
          case ArrayType(seq) => Try(seq.head.toString.charAt(0)) match {
            case Success(x) => CharType(x)
            case Failure(_) => throw TypeMismatch(seq.mkString, "Char", compilationUnit, position)
          }
          case value =>
            throw CastError(value.toString, "Char", compilationUnit, position)
        },
      args.size,
      1
    )(args, cu, pos)

  private def double(args: List[TopType], cu: CompilationUnit, pos: Position): TopType =
    BuiltinFunction(
      (args: List[TopType], compilationUnit: CompilationUnit, position: Position) =>
        args.head match {
          case IntegerType(v) => DoubleType(v.toDouble)
          case CharType(v) => DoubleType(v.toDouble)
          case DoubleType(v) => DoubleType(v)
          case ArrayType(seq) => Try(seq.mkString.toDouble) match {
            case Success(x) => DoubleType(x)
            case Failure(_) => throw TypeMismatch(seq.mkString, "Double", compilationUnit, position)
          }
          case value =>
            throw CastError(value.toString, "Double", compilationUnit, position)
        },
      args.size,
      1
    )(args, cu, pos)

  private def exit(args: List[TopType], cu: CompilationUnit, pos: Position): TopType =
    BuiltinFunction(
      (args: List[TopType], compilationUnit: CompilationUnit, position: Position) =>
        args.head match {
          case IntegerType(code) =>
            System.exit(code)
            UnitType
          case value =>
            throw CastError(value.toString, "Integer", compilationUnit, position)
        },
      args.size,
  1
    )(args, cu, pos)

  val functions: Map[String, BuiltinFunction] = Map(
    "println" -> println,
    "print" -> print,
    "size" -> size,
    "string" -> string,
    "read" -> read,
    "assert" -> assert,
    "int" -> int,
    "char" -> char,
    "double" -> double,
    "exit" -> exit
  )
}
