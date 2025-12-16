package inox.codegen

import inox.ast.BinaryOp
import inox.ir.*

import scala.collection.SortedSet

/** An emitter from Inox IR to C. */
object CEmitter {

  /** Emits the C representation of an Inox module. */
  def apply(module: Module): String = {
    val emitter = new CEmitter()
    val fnDecls = for { (name, function) <- module.toSeq } yield {
      emitter.emitFunction(name, function)
    }
    val fnTypes = emitter.fnTypes.map(t => s"typedef $t;\n\n").mkString
    val prototypes = fnDecls.map(d => d._1 + ";").mkString("\n\n")
    val decls = fnDecls.map(d => d._1 + " " + d._2).mkString("\n\n")
    s"$fnTypes$prototypes\n\n$decls"
  }
}

/** An emitter from Inox IR to C. */
private class CEmitter {

  /** The set of function pointer types that appear in the program being compiled to C. */
  private var fnTypes = SortedSet[String]()

  /** Emits the C representation of a function declaration. */
  private def emitFunction(name: String, function: Function): (String, String) = {
    def cDecl(local: Option[(Int, String)]): Option[String] =
      local.map((index, cType) => s"${cType} _${index}")

    val cLocals =
      function.locals.zipWithIndex.map((local, index) => emitType(local.ty).map(t => (index, t)))
    val cParams =
      cLocals
        .slice(1, function.paramCount + 1)
        .flatMap(cDecl)
        .mkString(", ")
    val cReturnType = cLocals(0).map(_._2).getOrElse("void")
    val cReturn = if (cLocals(0).isDefined) {
      "\n" + indent(1) + "return _0;"
    } else {
      ""
    }
    val cVars =
      (cLocals
        .slice(function.paramCount + 1, function.locals.length)
        .prepended(cLocals.head))
        .flatMap(cDecl(_).map("  " + _))
        .mkString(";\n") + ";\n"
    val cBody = emitBlock(function.locals, function.body, 1)
    (s"$cReturnType ${name}($cParams)", s"{\n$cVars$cBody$cReturn\n}")
  }

  /** Emits the C representation of a type. */
  private def emitType(ty: Type): Option[String] = ty.value.item match {
    case TypeKind.Fn(params, result) => {
      val (id, cType) = emitFnType(params, result)
      fnTypes = fnTypes ++ SortedSet(cType)
      Some(id)
    }
    case TypeKind.Ref(_, _, t)        => emitType(t).map(s => s"$s*")
    case TypeKind.I32 | TypeKind.Bool => Some("int")
    case TypeKind.Unit                => None
  }

  /** Emits the C representation of a function type. */
  private def emitFnType(params: Seq[Type], result: Type): (String, String) = {
    var cParams = params.flatMap(emitType)
    if (cParams.isEmpty) {
      cParams = Seq("void")
    }
    val cResult = emitType(result).getOrElse("void")
    val id = s"_${(cParams.mkString + cResult).hashCode.abs}"
    (id, s"$cResult (*$id)(${cParams.mkString(", ")})")
  }

  /** Emits the C representation of a block of instructions. */
  private def emitBlock(locals: IndexedSeq[Local], block: Block, level: Int): String =
    s"${block.flatMap(emitInstr(locals, _, level)).map(indent(level) + _).mkString("\n")}"

  /** Emits the C representation of an instruction. */
  private def emitInstr(locals: IndexedSeq[Local], instr: Instr, level: Int): Option[String] =
    instr match {
      case Instr.While(cond, body)            => emitWhile(locals, cond, body, level)
      case Instr.If(cond, thn, els)           => emitIf(locals, cond, thn, els, level)
      case Instr.Call(target, callee, args)   => emitCall(locals, target, callee, args)
      case Instr.Borrow(target, _, source)    => emitBorrow(locals, target, source)
      case Instr.Assign(target, value)        => emitAssignment(locals, target, value)
      case Instr.Binary(target, op, lhs, rhs) => emitBinary(locals, target, op, lhs, rhs)
      case Instr.Unary(target, op, operand)   => emitUnary(locals, target, op, operand)
      case Instr.Return                       => emitReturn(locals)
    }

  /** Emits the C representation of a while instruction. */
  private def emitWhile(
      locals: IndexedSeq[Local],
      cond: Operand,
      body: Block,
      level: Int
  ): Option[String] =
    for { cCond <- emitOperand(locals, cond) } yield {
      s"while ($cCond) {\n${emitBlock(locals, body, level + 1)}\n${indent(level)}}"
    }

  /** Emits the C representation of an if instruction. */
  private def emitIf(
      locals: IndexedSeq[Local],
      cond: Operand,
      thn: Block,
      els: Block,
      level: Int
  ): Option[String] = for { cCond <- emitOperand(locals, cond) } yield {
    s"if ($cCond) {\n${emitBlock(locals, thn, level + 1)}\n${indent(level)}} else {\n${emitBlock(locals, els, level + 1)}\n${indent(level)}}"
  }

  /** Emits the C representation of a call instruction. */
  private def emitCall(
      locals: IndexedSeq[Local],
      target: Place,
      callee: Operand,
      args: IndexedSeq[Operand]
  ): Option[String] = for { cCallee <- emitOperand(locals, callee) } yield {
    val cArgs = args.flatMap(emitOperand(locals, _)).mkString(", ")
    emitPlace(locals, target.item) match {
      case Some(cTarget) => s"$cTarget = $cCallee($cArgs);"
      case None          => s"$cCallee($cArgs);"
    }
  }

  /** Emits the C representation of a borrow instruction. */
  private def emitBorrow(locals: IndexedSeq[Local], target: Place, source: Place): Option[String] =
    for {
      cSource <- emitPlace(locals, source.item)
      cTarget <- emitPlace(locals, target.item)
    } yield {
      s"$cTarget = &$cSource;"
    }

  /** Emits the C representation of an assignment instruction. */
  private def emitAssignment(
      locals: IndexedSeq[Local],
      target: Place,
      value: Operand
  ): Option[String] = for {
    cTarget <- emitPlace(locals, target.item)
    cValue <- emitOperand(locals, value)
  } yield {
    s"$cTarget = $cValue;"
  }

  /** Emits the C representation of a binary instruction. */
  private def emitBinary(
      locals: IndexedSeq[Local],
      target: Place,
      op: BinaryOp,
      lhs: Operand,
      rhs: Operand
  ): Option[String] = for {
    cLhs <- emitOperand(locals, lhs)
    cRhs <- emitOperand(locals, rhs)
    cTarget <- emitPlace(locals, target.item)
  } yield {
    val cOp = op match {
      case BinaryOp.And => "&&"
      case BinaryOp.Or  => "||"
      case BinaryOp.Eq  => "=="
      case BinaryOp.Neq => "!="
      case BinaryOp.Lt  => "<"
      case BinaryOp.Le  => "<="
      case BinaryOp.Gt  => ">"
      case BinaryOp.Ge  => ">="
      case BinaryOp.Add => "+"
      case BinaryOp.Sub => "-"
      case BinaryOp.Mul => "*"
      case BinaryOp.Div => "/"
    }
    s"$cTarget = $cLhs $cOp $cRhs;"
  }

  /** Emits the C representation of a unary instruction. */
  private def emitUnary(
      locals: IndexedSeq[Local],
      target: Place,
      op: UnOp,
      operand: Operand
  ): Option[String] = for {
    cOperand <- emitOperand(locals, operand)
    cTarget <- emitPlace(locals, target.item)
  } yield {
    val cOp = op match {
      case UnOp.Not => "!"
      case UnOp.Neg => "-"
    }
    s"$cTarget = $cOp$cOperand;"
  }

  /** Emits the C representation of a return instruction. */
  private def emitReturn(locals: IndexedSeq[Local]): Option[String] =
    if (locals(0).ty.value.item == TypeKind.Unit) {
      Some("return;")
    } else {
      Some("return _0;")
    }

  /** Emits the C representation of an instruction operand. */
  private def emitOperand(locals: IndexedSeq[Local], operand: Operand): Option[String] =
    operand.item match {
      case OperandKind.Place(place)      => emitPlace(locals, place)
      case OperandKind.Fn(name, origins) => Some(name.item)
      case OperandKind.I32(value)        => Some(value.toString)
      case OperandKind.Bool(value)       => Some(if (value) "1" else "0")
      case OperandKind.Unit              => None
    }

  /** Emits the C representation of a place expression. */
  private def emitPlace(locals: IndexedSeq[Local], place: PlaceKind): Option[String] = place match {
    case PlaceKind.Deref(p) => emitPlace(locals, p.item).map(s => s"*$s")
    case PlaceKind.Var(id)  =>
      if (locals(id).ty.value.item != TypeKind.Unit) {
        Some(s"_$id")
      } else {
        None
      }
  }

  /** Emits indentation for a given level. */
  private def indent(level: Int): String = "  " * level
}
