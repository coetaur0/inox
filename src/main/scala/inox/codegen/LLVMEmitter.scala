package inox.codegen

import inox.ir.*
import org.bytedeco.javacpp.{Pointer, PointerPointer}
import org.bytedeco.llvm.LLVM.*
import org.bytedeco.llvm.global.LLVM.*

/** An emitter of LLVM IR for Inox. */
object LLVMEmitter {}

/** An emitter of LLVM IR for Inox. */
private class LLVMEmitter(context: LLVMContextRef, builder: LLVMBuilderRef) {

  private val module: LLVMModuleRef =
    LLVMModuleCreateWithNameInContext("main", context)
  private val intType = LLVMInt32TypeInContext(context)
  private var locals: IndexedSeq[Local] = IndexedSeq.empty
  private var localValues: Map[LocalId, LLVMValueRef] = Map.empty

  /** Emits the LLVM IR for a function prototype. */
  private def emitPrototype(function: inox.ir.Function): Unit = ???

  /** Emits the LLVM IR for a function. */
  private def emitFunction(function: inox.ir.Function): Unit = ???

  /** Emits the LLVM IR for a block. */
  private def emitBlock(block: Block): Unit = ???

  /** Emits the LLVM IR for an instruction. */
  private def emitInstr(instr: Instr): Unit = ???

  /** Emits the LLVM IR for an operand and returns an LLVM value for it. */
  private def emitOperand(operand: Operand): LLVMValueRef =
    operand.item match {
      case OperandKind.Place(place)      => emitPlace(place)._3
      case OperandKind.Fn(name, origins) =>
        LLVMGetNamedFunction(module, name.item)
      case OperandKind.I32(value)  => LLVMConstInt(intType, value, 0)
      case OperandKind.Bool(value) =>
        if (value) then LLVMConstInt(intType, 1, 0)
        else LLVMConstInt(intType, 0, 0)
      case OperandKind.Unit => LLVMConstNull(emitType(Type.Unit(operand.span)))
    }

  /** Emits the LLVM IR for a place and returns an LLVM value for it. */
  private def emitPlace(place: PlaceKind): (String, Type, LLVMValueRef) =
    place match {
      case PlaceKind.Deref(place) =>
        val (name, ty, value) = emitPlace(place.item)
        val derefName = s"*$name"
        val derefType = ty.value.item match {
          case TypeKind.Ref(_, _, ty) => ty
          case _                      => ty // Unreachable.
        }
        (
          derefName,
          derefType,
          LLVMBuildLoad2(builder, emitType(derefType), value, derefName)
        )
      case PlaceKind.Var(id) =>
        (locals(id).name.item, locals(id).ty, localValues(id))
    }

  /** Returns the LLVM representation of a type. */
  private def emitType(ty: Type): LLVMTypeRef =
    ty.value.item match {
      case TypeKind.Fn(params, result) =>
        val paramTypes = PointerPointer[Pointer](params.length)
        params.zipWithIndex.foreach { (param, index) =>
          paramTypes.put(index, emitType(param))
        }
        LLVMFunctionType(
          emitType(result),
          paramTypes,
          params.size,
          0
        )
      case TypeKind.Ref(_, _, ty)       => LLVMPointerType(emitType(ty), 0)
      case TypeKind.I32 | TypeKind.Bool => intType
      case TypeKind.Unit                => LLVMVoidTypeInContext(context)
    }

}
