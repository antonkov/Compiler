/**
  * Created by antonkov on 3/15/2016.
  */
import scala.util.parsing.combinator._
import org.bytedeco.javacpp._
import org.bytedeco.javacpp.LLVM._;

object Parser {
  def main(args: Array[String]): Unit = {
    var error: BytePointer = new BytePointer(null.asInstanceOf[Pointer])
    LLVMLinkInMCJIT
    LLVMInitializeNativeAsmPrinter
    LLVMInitializeNativeAsmParser
    LLVMInitializeNativeDisassembler
    LLVMInitializeNativeTarget
    val mod: LLVM.LLVMModuleRef = LLVMModuleCreateWithName("fac_module")
    val fac_args: Array[LLVM.LLVMTypeRef] = Array(LLVMInt32Type)
    val fac: LLVM.LLVMValueRef = LLVMAddFunction(mod, "fac", LLVMFunctionType(LLVMInt32Type, fac_args(0), 1, 0))
    LLVMSetFunctionCallConv(fac, LLVMCCallConv)
    val n: LLVM.LLVMValueRef = LLVMGetParam(fac, 0)

    val entry: LLVM.LLVMBasicBlockRef = LLVMAppendBasicBlock(fac, "entry")
    val iftrue: LLVM.LLVMBasicBlockRef = LLVMAppendBasicBlock(fac, "iftrue")
    val iffalse: LLVM.LLVMBasicBlockRef = LLVMAppendBasicBlock(fac, "iffalse")
    val end: LLVM.LLVMBasicBlockRef = LLVMAppendBasicBlock(fac, "end")
    val builder: LLVM.LLVMBuilderRef = LLVMCreateBuilder

    LLVMPositionBuilderAtEnd(builder, entry)
    val If: LLVM.LLVMValueRef = LLVMBuildICmp(builder, LLVMIntEQ, n, LLVMConstInt(LLVMInt32Type, 0, 0), "n == 0")
    LLVMBuildCondBr(builder, If, iftrue, iffalse)

    LLVMPositionBuilderAtEnd(builder, iftrue)
    val res_iftrue: LLVM.LLVMValueRef = LLVMConstInt(LLVMInt32Type, 1, 0)
    LLVMBuildBr(builder, end)

    LLVMPositionBuilderAtEnd(builder, iffalse)
    val n_minus: LLVM.LLVMValueRef = LLVMBuildSub(builder, n, LLVMConstInt(LLVMInt32Type, 1, 0), "n - 1")
    val call_fac_args: Array[LLVM.LLVMValueRef] = Array(n_minus)
    val call_fac: LLVM.LLVMValueRef = LLVMBuildCall(builder, fac, new PointerPointer[LLVM.LLVMValueRef](call_fac_args:_*), 1, "fac(n - 1)")
    val res_iffalse: LLVM.LLVMValueRef = LLVMBuildMul(builder, n, call_fac, "n * fac(n - 1)")
    LLVMBuildBr(builder, end)

    LLVMPositionBuilderAtEnd(builder, end)
    val res: LLVM.LLVMValueRef = LLVMBuildPhi(builder, LLVMInt32Type, "result")
    val phi_vals: Array[LLVM.LLVMValueRef] = Array(res_iftrue, res_iffalse)
    val phi_blocks: Array[LLVM.LLVMBasicBlockRef] = Array(iftrue, iffalse)
    LLVMAddIncoming(res, new PointerPointer[LLVM.LLVMValueRef](phi_vals:_*), new PointerPointer[LLVM.LLVMBasicBlockRef](phi_blocks:_*), 2)
    LLVMBuildRet(builder, res)

    LLVMVerifyModule(mod, LLVMAbortProcessAction, error)
    LLVMDisposeMessage(error)


    val engine: LLVM.LLVMExecutionEngineRef = new LLVM.LLVMExecutionEngineRef
    val provider: LLVM.LLVMModuleProviderRef = LLVMCreateModuleProviderForExistingModule(mod)
    error = new BytePointer(null.asInstanceOf[Pointer])
    if (LLVMCreateJITCompiler(engine, provider, 2, error) != 0) {
      System.err.println(error.getString)
      LLVMDisposeMessage(error)
      System.exit(-1)
    }

    val pass: LLVM.LLVMPassManagerRef = LLVMCreatePassManager
    LLVMAddTargetData(LLVMGetExecutionEngineTargetData(engine), pass)
    LLVMAddConstantPropagationPass(pass)
    LLVMAddInstructionCombiningPass(pass)
    LLVMAddPromoteMemoryToRegisterPass(pass)
    // LLVMAddDemoteMemoryToRegisterPass(pass); // Demotes every possible value to memory
    LLVMAddGVNPass(pass)
    LLVMAddCFGSimplificationPass(pass)
    LLVMRunPassManager(pass, mod)
    LLVMDumpModule(mod)

    val exec_args: LLVM.LLVMGenericValueRef = LLVMCreateGenericValueOfInt(LLVMInt32Type, 3, 0)
    val exec_res: LLVM.LLVMGenericValueRef = LLVMRunFunction(engine, fac, 1, exec_args)
    System.err.println
    System.err.println("; Running fac(10) with JIT...")
    System.err.println("; Result: " + LLVMGenericValueToInt(exec_res, 0))

    LLVMDisposePassManager(pass)
    LLVMDisposeBuilder(builder)
    LLVMDisposeExecutionEngine(engine)
  }
}