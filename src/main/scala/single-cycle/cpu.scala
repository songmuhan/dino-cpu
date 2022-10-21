// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.4 of Patterson and Hennessy
 * This follows figure 4.21
 */
class SingleCycleCPU(implicit val conf: CPUConfig) extends BaseCPU {
  // All of the structures required
  val pc         = dontTouch(RegInit(0.U))
  val control    = Module(new Control())
  val registers  = Module(new RegisterFile())
  val aluControl = Module(new ALUControl())
  val alu        = Module(new ALU())
  val immGen     = Module(new ImmediateGenerator())
  val nextpc     = Module(new NextPC())
  val (cycleCount, _) = Counter(true.B, 1 << 30)
  


  // Should be removed when wired are connected
  /*
  control.io    := DontCare
  registers.io  := DontCare
  aluControl.io := DontCare
  alu.io        := DontCare
  immGen.io     := DontCare
  nextpc.io     := DontCare
  io.dmem       := DontCare
 */
  immGen.io     := DontCare
  io.dmem       := DontCare
 
  //FETCH
  io.imem.address := pc
  io.imem.valid := true.B

  val instruction = io.imem.instruction
  // Your code goes here
  val rs1_index = instruction(19,15)
  val rs2_index = instruction(24,20)
  val rd_index  = instruction(11,7)
  val opcode    = instruction(6,0)
  val funct7    = instruction(31,25)
  val funct3    = instruction(14,12)
  val read_data_1 = registers.io.readdata1
  val read_data_2 = registers.io.readdata2

  nextpc.io.branch := false.B
  nextpc.io.jumptype := false.B
  nextpc.io.inputx := read_data_1
  nextpc.io.inputy := read_data_2
  nextpc.io.pc := pc
  nextpc.io.funct3 := funct3
  nextpc.io.imm := 0.U 
  
  pc := nextpc.io.nextpc





  
  // control unit input
  control.io.opcode      := opcode
  // aluControl input
  aluControl.io.aluop    := control.io.aluop
  aluControl.io.itype    := control.io.itype
  aluControl.io.funct7   := funct7
  aluControl.io.funct3   := funct3
  aluControl.io.wordinst := control.io.wordinst


  // register file input
  registers.io.readreg1 := rs1_index
  registers.io.readreg2 := rs2_index
  registers.io.writereg := rd_index
  registers.io.wen := control.io.regwrite


  
  // alu input
  alu.io.inputx := read_data_1
  alu.io.inputy := read_data_2
  alu.io.operation := aluControl.io.operation

  val aluresult = alu.io.result
  
  when(rd_index === 0.U){
    registers.io.writedata := 0.U
  }
  .otherwise{
    registers.io.writedata := aluresult
  }
//  printf(p"write data ${aluresult}, wirte index${rd_index}")




}

/*
 * Object to make it easier to print information about the CPU
 */
object SingleCycleCPUInfo {
  def getModules(): List[String] = {
    List(
      "dmem",
      "imem",
      "control",
      "registers",
      "csr",
      "aluControl",
      "alu",
      "immGen",
      "nextpc"
    )
  }
}
