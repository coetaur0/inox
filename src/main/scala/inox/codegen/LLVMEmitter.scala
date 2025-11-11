package inox.codegen

import inox.ir.*
import org.bytedeco.javacpp.{Pointer, PointerPointer}
import org.bytedeco.llvm.LLVM.*
import org.bytedeco.llvm.global.LLVM.*

/** An LLVM IR emitter for Inox. */
object LLVMEmitter {}

/** An LLVM IR emitter for Inox. */
private class LLVMEmitter(context: LLVMContextRef, builder: LLVMBuilderRef) {
  private val module: LLVMModuleRef = LLVMModuleCreateWithNameInContext("main", context)

  private var locals: IndexedSeq[Local]             = IndexedSeq.empty
  private var localValues: IndexedSeq[LLVMValueRef] = IndexedSeq.empty

  private val intType  = LLVMInt32TypeInContext(context)
  private val boolType = LLVMInt8TypeInContext(context)
  private val unitType = LLVMInt1TypeInContext(context)
  private val unit     = LLVMConstInt(unitType, 0, 0)

  /** Emits LLVM IR for a function prototype. */
  private def emitPrototype(function: inox.ir.Function): Unit = ???

  /** Emits LLVM IR for a function. */
  private def emitFunction(function: inox.ir.Function): Unit = ???

  /** Emits LLVM IR for a block. */
  private def emitBlock(block: Block): Unit = ???

  /** Emits LLVM IR for an instruction. */
  private def emitInstr(instr: Instr): Unit =
    instr match {
      case inox.ir.Instr.While(cond, body)               => ???
      case inox.ir.Instr.If(cond, thn, els)              => ???
      case inox.ir.Instr.Call(target, callee, args)      => ???
      case inox.ir.Instr.Borrow(target, mutable, source) => ???
      case inox.ir.Instr.Assign(target, value)           => ???
      case inox.ir.Instr.Binary(target, op, lhs, rhs)    => ???
      case inox.ir.Instr.Unary(target, op, operand)      =>
        // TODO: store the result in target.
        op match {
          case UnOp.Not =>
            LLVMBuildXor(builder, emitOperand(operand), LLVMConstInt(boolType, 1, 0), "")
          case UnOp.Neg =>
            LLVMBuildSub(builder, LLVMConstInt(intType, 0, 0), emitOperand(operand), "")
        }
      case inox.ir.Instr.Return => LLVMBuildRet(builder, localValues(0))
    }

  /** Emits LLVM IR for an operand and returns an LLVM value for it. */
  private def emitOperand(operand: Operand): LLVMValueRef = operand.item match {
    case OperandKind.Place(place) => emitPlace(place)._3
    case OperandKind.Fn(name, _)  => LLVMGetNamedFunction(module, name.item)
    case OperandKind.I32(value)   => LLVMConstInt(intType, value, 0)
    case OperandKind.Bool(value)  =>
      if (value) {
        LLVMConstInt(boolType, 1, 0)
      } else {
        LLVMConstInt(boolType, 0, 0)
      }
    case OperandKind.Unit => unit
  }

  /** Emits LLVM IR for a place expression and returns an LLVM value for it. */
  private def emitPlace(place: PlaceKind): (String, Type, LLVMValueRef) = place match {
    case PlaceKind.Deref(place) => {
      val (name, ty, value) = emitPlace(place.item)
      val derefName         = s"*$name"
      val derefType         = ty.value.item match {
        case TypeKind.Ref(_, _, ty) => ty
        case _                      => ty // Unreachable.
      }
      (
        derefName,
        derefType,
        LLVMBuildLoad2(builder, emitType(derefType), value, derefName)
      )
    }
    case PlaceKind.Var(id) => (locals(id).name.item, locals(id).ty, localValues(id))
  }

  /** Returns the LLVM representation of a type. */
  private def emitType(ty: Type): LLVMTypeRef = ty.value.item match {
    case TypeKind.Fn(params, result) => {
      val paramTypes = PointerPointer[Pointer](params.length)
      params.zipWithIndex.foreach { (param, index) => paramTypes.put(index, emitType(param)) }
      LLVMFunctionType(emitType(result), paramTypes, params.size, 0)
    }
    case TypeKind.Ref(_, _, ty) => LLVMPointerType(emitType(ty), 0)
    case TypeKind.I32           => intType
    case TypeKind.Bool          => boolType
    case TypeKind.Unit          => unitType
  }
}
