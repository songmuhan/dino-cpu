// This file contains the forwarding unit

package dinocpu.components.dual

import chisel3._

/**
 * The Forwarding unit with support for dual-issue pipeline
 *
 * Input:  ex_pipeA_rs1, execute stage pipeA source register 1
 * Input:  ex_pipeA_rs2, execute stage pipeA source register 2
 * Input:  ex_pipeB_rs1, execute stage pipeB source register 1
 * Input:  ex_pipeB_rs2, execute stage pipeB source register 2
 * Input:  ex_pipeA_rd,  execute stage pipeA destination register
 * Input:  ex_pipeA_rw,  True if execute stage pipeA writes back to register file
 * Input:  mem_pipeA_rd, memory stage pipeA destination register
 * Input:  mem_pipeA_rw, True if memory stage pipeA writes back to register file
 * Input:  mem_pipeB_rd, memory stage pipeB destination register
 * Input:  mem_pipeB_rw, True if memory stage pipeB writes back to register file
 * Input:  wb_pipeA_rd,  writeback stage pipeA destination register
 * Input:  wb_pipeA_rw,  True if writeback stage pipeA writes back to register file
 * Input:  wb_pipeB_rd,  writeback stage pipeB destination register
 * Input:  wb_pipeB_rw,  True if writeback stage pipeB writes back to register file
 *
 * Output: pipeA_forwardA, 0, don't forward
 *                         1, forward from mem stage pipeA
 *                         2, forward from mem stage pipeB
 *                         3, forward from wb stage pipeA
 *                         4, forward from wb stage pipeB
 *                         This is used for the "readdata1" forwarding
 * Output: pipeA_forwardB, 0, don't forward
 *                         1, forward from mem stage pipeA
 *                         2, forward from mem stage pipeB
 *                         3, forward from wb stage pipeA
 *                         4, forward from wb stage pipeB
 *                         This is used for the "readdata2" forwarding
 *
 * Output: pipeB_forward1, 0, don't forward
 *                         1, forward from ex stage pipeA // NOT IMPLEMENTED
 *                         2, forward from mem stage pipeA
 *                         3, forward from mem stage pipeB
 *                         4, forward from wb stage pipeA
 *                         5, forward from wb stage pipeB
 *                         This is used for the "readdata1" forwarding
 * Output: pipeB_forward2, 0, don't forward
 *                         1, forward from ex stage pipeA // NOT IMPLEMENTED
 *                         2, forward from mem stage pipeA
 *                         3, forward from mem stage pipeB
 *                         4, forward from wb stage pipeA
 *                         5, forward from wb stage pipeB
 *                         This is used for the "readdata2" forwarding
 */

class ForwardingUnit extends Module {
  val io = IO(new Bundle {
    val ex_pipeA_rs1 = Input(UInt(5.W))
    val ex_pipeA_rs2 = Input(UInt(5.W))
    val ex_pipeB_rs1 = Input(UInt(5.W))
    val ex_pipeB_rs2 = Input(UInt(5.W))

    val ex_pipeA_rd  = Input(UInt(5.W))
    val ex_pipeA_rw  = Input(Bool())
    val mem_pipeA_rd = Input(UInt(5.W))
    val mem_pipeA_rw = Input(Bool())
    val mem_pipeB_rd = Input(UInt(5.W))
    val mem_pipeB_rw = Input(Bool())
    val wb_pipeA_rd  = Input(UInt(5.W))
    val wb_pipeA_rw  = Input(Bool())
    val wb_pipeB_rd  = Input(UInt(5.W))
    val wb_pipeB_rw  = Input(Bool())

    val pipeA_forward1 = Output(UInt(3.W))
    val pipeA_forward2 = Output(UInt(3.W))
    val pipeB_forward1 = Output(UInt(3.W))
    val pipeB_forward2 = Output(UInt(3.W))
  })

  // pipeA_forward1
  io.pipeA_forward1 := 0.U



  // pipeA_forward2
  io.pipeA_forward2 := 0.U



  // pipeB_forward1
  io.pipeB_forward1 := 0.U



  // pipeB_forward2
  io.pipeB_forward2 := 0.U

}
