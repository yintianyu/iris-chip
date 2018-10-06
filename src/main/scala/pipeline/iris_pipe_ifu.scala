
package pipeline

import chisel3._
import chisel3.util._

class Interface_ifu2mem(n:Int) extends Bundle{
    val imem_rdata = Flipped(Decoupled(UInt(n.W)))
    val imem_addr = Decoupled(UInt(n.W))
}

class Interface_ifu2newpc(n:Int) extends Bundle{
    val new_pc = Flipped(Decoupled(UInt(n.W)))
    val stop_fetch = Input(Bool())
}

class Interface_ifu2idu(n:Int) extends Bundle{
    val ifu_busy = Output(Bool())
    val ifu2idu_instr = Decoupled(UInt(n.W))
}

class Iris_pipe_ifu(n:Int) extends Module{
    val io = IO(new Bundle{
        val ifu2mem = new Interface_ifu2mem(n)
        val ifu2newpc = new Interface_ifu2newpc(n)
        val ifu2idu = new Interface_ifu2idu(n)
    })

    val iris_pipe_ifu_idle :: iris_pipe_ifu_fetch :: Nil = Enum(2)
    val instr = RegInit(0.U(n.W))
    val fsm_status = RegInit(iris_pipe_ifu_idle)
    val fetch = Wire(Bool())
    fetch := !io.ifu2newpc.stop_fetch //&& (fsm_status === iris_pipe_ifu_idle)

    // FSM
    when(fsm_status === iris_pipe_ifu_idle && io.ifu2newpc.new_pc.valid && !io.ifu2newpc.stop_fetch)
    {
        fsm_status := iris_pipe_ifu_fetch
    }
    .elsewhen(fsm_status === iris_pipe_ifu_fetch && io.ifu2newpc.stop_fetch)
    {
        fsm_status := iris_pipe_ifu_idle
    }


    io.ifu2idu.ifu_busy := fsm_status === iris_pipe_ifu_fetch

    io.ifu2mem.imem_addr.valid := false.B
    io.ifu2idu.ifu2idu_instr.valid := false.B
    when(io.ifu2mem.imem_addr.ready && (fsm_status === iris_pipe_ifu_fetch))
    {
        io.ifu2mem.imem_addr.bits := io.ifu2newpc.new_pc.bits
        io.ifu2mem.imem_addr.valid := true.B
    }
    when(io.ifu2mem.imem_rdata.ready)
    {
        instr := io.ifu2mem.imem_rdata.bits
    }
    when(io.ifu2idu.ifu2idu_instr.ready)
    {
        io.ifu2idu.ifu2idu_instr.valid := true.B
        io.ifu2idu.ifu2idu_instr.bits := instr
    }
}