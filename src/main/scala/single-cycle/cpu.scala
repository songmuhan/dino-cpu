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
  io.dmem       := DontCare
 
  //FETCH
  io.imem.address := pc
  io.imem.valid := true.B

  val instruction = io.imem.instruction
  // Your code goes here
  // instruction
  val instr_rs1_index = instruction(19,15)
  val instr_rs2_index = instruction(24,20)
  val instr_rd_index  = instruction(11,7)
  val instr_opcode    = instruction(6,0)
  val instr_funct7    = instruction(31,25)
  val instr_funct3    = instruction(14,12)
  // register file
  val reg_read_data_1 = registers.io.readdata1
  val reg_read_data_2 = registers.io.readdata2
  // alu
  val alu_result = alu.io.result
  // alu control
  val aluctr_op = aluControl.io.operation
  // imm
  val imm_result = immGen.io.sextImm
  // control
  val ctr_itype = control.io.itype
  val ctr_aluop = control.io.aluop
  val ctr_src1  = control.io.src1
  val ctr_src2  = control.io.src2
  val ctr_branch = control.io.branch
  val ctr_jumptype = control.io.jumptype
  val ctr_resultselect = control.io.resultselect
  val ctr_memop = control.io.memop
  val ctr_toreg = control.io.toreg
  val ctr_regwrite = control.io.regwrite
  val ctr_validinst = control.io.validinst
  val ctr_wordinst = control.io.wordinst
  // data memory
  val mem_readdata = io.dmem.readdata 

  // mux
  // alu
  val alu_mux_inputx = Mux(ctr_src1, pc, reg_read_data_1)
  val alu_mux_inputy = MuxCase(reg_read_data_2, 
                                Array(
                                    (ctr_src2 === "b00".U) -> reg_read_data_2,
                                    (ctr_src2 === "b01".U) -> imm_result,
                                    (ctr_src2 === "b10".U) -> 4.U
                               ))
  val nextpc_mux_inputy = alu_mux_inputy
  val alu_mux_result = Mux(ctr_resultselect,imm_result,alu_result)
  val register_mux_write_data = Mux(ctr_toreg,mem_readdata,alu_mux_result)

/*
  printf(p"control: resultselect ${ctr_resultselect}, toreg ${ctr_toreg}\n");
  printf(p"memory : mem_readdata ${mem_readdata}\n");

  printf(p"register : register_mux_write_data ${register_mux_write_data}\n");
*/
/*
    deal with nextpc part
*/
  nextpc.io.branch   := ctr_branch
  nextpc.io.jumptype := ctr_jumptype
  nextpc.io.inputx   := reg_read_data_1
  nextpc.io.inputy   := nextpc_mux_inputy
  nextpc.io.pc       := pc
  nextpc.io.funct3   := instr_funct3
  nextpc.io.imm      := imm_result
  pc := nextpc.io.nextpc

/*
    deal with control unit
*/
  control.io.opcode    := instr_opcode

/*
    deal with aluControl
*/
  aluControl.io.aluop    := ctr_aluop
  aluControl.io.itype    := ctr_itype
  aluControl.io.funct7   := instr_funct7
  aluControl.io.funct3   := instr_funct3
  aluControl.io.wordinst := ctr_wordinst


/*
    deal with ImmGenerator
*/
  immGen.io.instruction := instruction

/*
    deal with register file
*/
  registers.io.readreg1 := instr_rs1_index
  registers.io.readreg2 := instr_rs2_index
  registers.io.writereg := instr_rd_index
  registers.io.wen := ctr_regwrite
  when(registers.io.writereg === "b00000".U){
    registers.io.writedata := 0.U
  }
  .otherwise{
    registers.io.writedata := register_mux_write_data
  }
/*
    deal with register file
*/
  alu.io.operation := aluctr_op
  alu.io.inputx := alu_mux_inputx
  alu.io.inputy := alu_mux_inputy
/*
    deal with data memory
*/                          
  io.dmem.address := alu_result
  when(ctr_memop === "b10".U){  // read memory
    // load type
    io.dmem.memread := true.B
    io.dmem.memwrite := false.B
    io.dmem.valid := true.B
    io.dmem.sext := true.B
    when(instr_funct3 === "b000".U){  // lb
        io.dmem.maskmode := 0.U // 0 -> byte,1-> halfword,2->word, 3 double
    }.elsewhen(instr_funct3 === "b001".U){ // lh
        io.dmem.maskmode := 1.U
    }.elsewhen(instr_funct3 === "b010".U){ // lw
        io.dmem.maskmode := 2.U
    }.elsewhen(instr_funct3 === "b011".U){  //ld
        io.dmem.maskmode := 3.U
    }.elsewhen(instr_funct3 === "b100".U){ //lbu
        io.dmem.maskmode := 0.U
        io.dmem.sext := false.B
    }.elsewhen(instr_funct3 === "b101".U){ // lhu
        io.dmem.maskmode := 1.U
        io.dmem.sext := false.B
    }.elsewhen(instr_funct3 === "b110".U){ // lwu
        io.dmem.maskmode := 2.U
        io.dmem.sext := false.B
    }
    .otherwise{
        io.dmem.maskmode := 0.U // error
    }
  }.elsewhen(ctr_memop === "b11".U){ // store memory
      io.dmem.memread := false.B
      io.dmem.memwrite := true.B
      io.dmem.valid := true.B
    when(instr_funct3==="b000".U){ // sb
        io.dmem.maskmode := 0.U
    }.elsewhen(instr_funct3 === "b001".U){ // sh
        io.dmem.maskmode := 1.U
    }.elsewhen(instr_funct3 === "b010".U){ //sw
        io.dmem.maskmode := 2.U
    }.elsewhen(instr_funct3 === "b011".U){ //sd
        io.dmem.maskmode := 3.U
    }.otherwise{
        io.dmem.maskmode := 0.U
    }

  }.otherwise{ // do nothing
      io.dmem.memread := false.B
      io.dmem.memwrite := false.B
      io.dmem.valid := false.B
  }

  io.dmem.writedata := reg_read_data_2

  

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
