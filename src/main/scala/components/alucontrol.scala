// This file contains ALU control logic.

package dinocpu.components

import chisel3._
import chisel3.util._

/**
 * The ALU control unit
 *
 * Input:  aluop, 0 for ld/st, 1 for R-type
 * Input:  funct7, the most significant bits of the instruction
 * Input:  funct3, the middle three bits of the instruction (12-14)
 * Input:  wordinst, True if the instruction *only* operates on 32-bit operands, False otherwise
 * Output: operation, What we want the ALU to do.
 *
 * For more information, see Section 4.4 and A.5 of Patterson and Hennessy.
 * This is loosely based on figure 4.12
 */
class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluop     = Input(Bool())
    val itype     = Input(Bool())
    val funct7    = Input(UInt(7.W))
    val funct3    = Input(UInt(3.W))
    val wordinst  = Input(Bool())

    val operation = Output(UInt(5.W))
  })

  // Your code goes here'
  io.operation := "b00110".U // add 
  when(io.wordinst === false.B){
    when(io.aluop === true.B){ // R type
            when(io.funct3 === "b000".U && io.funct7 === "b0000000".U){
                io.operation := "b00111".U // add 
            }
            .elsewhen(io.funct3 === "b000".U && io.funct7 === "b0100000".U){
                io.operation := "b00100".U // sub
            }
            .elsewhen(io.funct3 === "b001".U && io.funct7 === "b0000000".U){
                io.operation := "b01000".U // sll
            }
            .elsewhen(io.funct3 === "b010".U && io.funct7 === "b0000000".U){
                io.operation := "b01001".U // slt
            }
            .elsewhen(io.funct3 === "b011".U && io.funct7 === "b0000000".U){
                io.operation := "b00001".U // sltu
            }
            .elsewhen(io.funct3 === "b100".U && io.funct7 === "b0000000".U){
                io.operation := "b00000".U // xor
            }
            .elsewhen(io.funct3 === "b101".U && io.funct7 === "b0000000".U){
                io.operation := "b00010".U // srl
            }
            .elsewhen(io.funct3 === "b101".U && io.funct7 === "b0100000".U){
                io.operation := "b00011".U // sra
            }
            .elsewhen(io.funct3 === "b110".U && io.funct7 === "b0000000".U){
                io.operation := "b00101".U // or
            }
            .elsewhen(io.funct3 === "b111".U && io.funct7 === "b0000000".U){
                io.operation := "b00110".U // and
            }
    }
    .otherwise{
             io.operation := "b00111".U // ld and store -> add 
    }
  }
  .elsewhen(io.wordinst === true.B){
        when(io.aluop === true.B){
            when(io.funct3 === "b000".U && io.funct7 === "b0000000".U){
                io.operation := "b10111".U // addw 
            }
            .elsewhen(io.funct3 === "b000".U && io.funct7 === "b0100000".U){
                io.operation := "b10100".U  // subw
            }
            .elsewhen(io.funct3 === "b001".U && io.funct7 === "b0000000".U){
                io.operation := "b11000".U  // sllw
            }
            .elsewhen(io.funct3 === "b101".U && io.funct7 === "b0000000".U){
                io.operation := "b10010".U  // srlw
            }
            .elsewhen(io.funct3 === "b101".U && io.funct7 === "b0100000".U){
                io.operation := "b10011".U  // sraw
            }
        }
        .otherwise{
             io.operation := "b10111".U // load & store -> addw
        }
  }
// printf(p"operation->${io.operation},wordinst->${io.wordinst},aluop -> ${io.aluop}, funct3->${io.funct3}, funct7->${io.funct7}\n")

 
  


  //io.operation := "b11111".U // invalid operation
}
