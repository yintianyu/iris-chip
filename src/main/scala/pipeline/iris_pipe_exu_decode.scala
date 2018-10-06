
package pipeline

import chisel3._
import chisel3.util._
import typedef._
import Constants._
import Instructions._
import org.scalacheck.Prop.True

// TODO: Support A M C extension

class Iris_pipe_exu_decode(pipeline_dec : Boolean) extends Module{
    val io = IO(new Bundle{
        val i_instr = Input(UInt(IRIS_INSTR_SIZE.W))
        val i_pc = Input(UInt(IRIS_PC_SIZE.W))
        val i_predict_taken = Input(Bool())
        val i_misalign = Input(Bool())
        val i_buserr = Input(Bool())
        val i_muldiv_b2b = Input(Bool())

        val dbg_mode = Input(Bool())

        val dec_rs1x0 = Output(Bool())
        val dec_rs2x0 = Output(Bool())
        val dec_rs1en = Output(Bool())
        val dec_rs2en = Output(Bool())
        val dec_rdwen = Output(Bool())
        val dec_rs1idx = Output(UInt(IRIS_RFIDX_WIDTH.W))
        val dec_rs2idx = Output(UInt(IRIS_RFIDX_WIDTH.W))
        val dec_rdidx = Output(UInt(IRIS_RFIDX_WIDTH.W))
        val dec_info = Output(UInt(IRIS_DECINFO_WIDTH.W))
        val dec_imm = Output(UInt(IRIS_XLEN.W))
        val dec_pc = Output(UInt(IRIS_PC_SIZE.W))
        val dec_misalign = Output(Bool())
        val dec_buserr = Output(Bool())
        val dec_illegl = Output(Bool())

        val dec_mulhsu = Output(Bool())
        val dec_mul = Output(Bool())
        val dec_div = Output(Bool())
        val dec_rem = Output(Bool())
        val dec_divu = Output(Bool())
        val dec_remu = Output(Bool())

        val dec_rv32 = Output(Bool())
        val dec_bjp = Output(Bool())
        val dec_jal = Output(Bool())
        val dec_jalr = Output(Bool())
        val dec_bxx = Output(Bool())

        val dec_jalr_rs1idx = Output(UInt(IRIS_RFIDX_WIDTH.W))
        val dec_bjp_imm = Output(UInt(IRIS_XLEN.W))
    })

    // Pipeline or not?
    val instr = if (pipeline_dec == true) RegInit(io.i_instr) else WireInit(io.i_instr)
    val pc = if(pipeline_dec == true) RegInit(io.i_pc) else WireInit(io.i_pc)
    val misalign = if(pipeline_dec == true) RegInit(io.i_misalign) else WireInit(io.i_misalign)
    val buserr = if(pipeline_dec == true) RegInit(io.i_buserr) else WireInit(io.i_buserr)
    io.dec_pc := pc
    io.dec_misalign := misalign
    io.dec_buserr := buserr

    val rv32_instr = WireInit(instr)
    val rv16_instr = WireInit(instr(15,0))

    val rvi_signals =
        ListLookup(instr,
            List(N, BR_N  , OP1_X , OP2_X    , OEN_0, OEN_0, ALU_X   , WB_X  ,  REN_0, MEN_0, M_X  , MT_X, CSR.N, N),
            Array(        /* val  |  BR  |  op1  |   op2     |  R1  |  R2  |  ALU    |  wb   | rf   | mem  | mem  | mask | csr | fence.i | RV32I  */
                          /* inst | type |   sel |    sel    |  oen |  oen |   fcn   |  sel  | wen  |  en  |  wr  | type | cmd |         | type   */
                LW     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_W, CSR.N, N, RV32I_LW      ),
                LB     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_B, CSR.N, N, RV32I_LB      ),
                LBU    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_BU,CSR.N, N, RV32I_LBU     ),
                LH     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_H, CSR.N, N, RV32I_LH      ),
                LHU    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_HU,CSR.N, N, RV32I_LHU     ),
                SW     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_W, CSR.N, N, RV32I_SW      ),
                SB     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_B, CSR.N, N, RV32I_SB      ),
                SH     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_H, CSR.N, N, RV32I_SH      ),

                AUIPC  -> List(Y, BR_N  , OP1_PC , OP2_UTYPE , OEN_0, OEN_0, ALU_ADD   ,WB_ALU,REN_1, MEN_0, M_X , MT_X,  CSR.N, N, RV32I_AUIPC   ),
                LUI    -> List(Y, BR_N  , OP1_X  , OP2_UTYPE , OEN_0, OEN_0, ALU_COPY_2,WB_ALU,REN_1, MEN_0, M_X , MT_X,  CSR.N, N, RV32I_LUI     ),

                ADDI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_ADDI    ),
                ANDI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_AND , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_ANDI    ),
                ORI    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_OR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_ORI     ),
                XORI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_XOR , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_XORI    ),
                SLTI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLT , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_SLTI    ),
                SLTIU  -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLTU, WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_SLTIU   ),
                SLLI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_SLLI    ),
                SRAI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SRA , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_SRAI    ),
                SRLI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SRL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_SRLI    ),

                SLL    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_SLL     ),
                ADD    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_ADD     ),
                SUB    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SUB , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_SUB     ),
                SLT    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLT , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_SLT     ),
                SLTU   -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLTU, WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_SLTU    ),
                AND    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_AND , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_AND     ),
                OR     -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_OR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_OR      ),
                XOR    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_XOR , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_XOR     ),
                SRA    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SRA , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_SRA     ),
                SRL    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SRL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_SRL     ),

                JAL    -> List(Y, BR_J  , OP1_RS1, OP2_UJTYPE, OEN_0, OEN_0, ALU_X   , WB_PC4, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_JAL     ),
                JALR   -> List(Y, BR_JR , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_X   , WB_PC4, REN_1, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_JALR    ),
                BEQ    -> List(Y, BR_EQ , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_BEQ     ),
                BNE    -> List(Y, BR_NE , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_BNE     ),
                BGE    -> List(Y, BR_GE , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_BGE     ),
                BGEU   -> List(Y, BR_GEU, OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_BGEU    ),
                BLT    -> List(Y, BR_LT , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_BLT     ),
                BLTU   -> List(Y, BR_LTU, OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_BLTU    ),

                CSRRWI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.W, N, RV32I_CSRRWI  ),
                CSRRSI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.S, N, RV32I_CSRRSI  ),
                CSRRW  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.W, N, RV32I_CSRRW   ),
                CSRRS  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.S, N, RV32I_CSRRS   ),
                CSRRC  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.C, N, RV32I_CSRRC   ),
                CSRRCI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.C, N, RV32I_CSRRCI  ),

                ECALL  -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N, RV32I_ECALL   ),
                MRET   -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N, RV32I_MRET    ),
                DRET   -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N, RV32I_DRET    ),
                EBREAK -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N, RV32I_EBREAK  ),
                WFI    -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N, RV32I_WFI     ), // implemented as a NOP

                FENCE_I-> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, Y, RV32I_FENCE_I ),
                // kill pipeline and refetch instructions since the pipeline will be holding stall instructions.
                FENCE  -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_1, M_X  , MT_X, CSR.N, N, RV32I_FENCE   )
                // we are already sequentially consistent, so no need to honor the fence instruction


            ))

    // Put these control signals in variables
    val (cs_val_inst: Bool) :: cs_br_type :: cs_op1_sel :: cs_op2_sel :: (cs_rs1_oen: Bool) :: (cs_rs2_oen: Bool) :: cs0 = rvi_signals
    val cs_alu_fun :: cs_wb_sel :: (cs_rf_wen: Bool) :: (cs_mem_en: Bool) :: cs_mem_fcn :: cs_msk_sel :: cs_csr_cmd :: (cs_fencei: Bool) :: cs_instr_type :: Nil = cs0


    // Deal with imm
    val rv32_i_imm = WireInit(Cat(Fill(20, instr(31)), instr(31, 20)))
    val rv32_s_imm = WireInit(Cat(Fill(20, instr(31)), instr(31, 25), instr(11, 7)))
    val rv32_u_imm = WireInit(Cat(Fill(10, instr(31)), instr(31), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)))
    val rv32_j_imm = WireInit(Cat(Fill(12, instr(31)), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W)))
    val rv32_jal_imm = WireInit(rv32_j_imm)
    val rv32_jalr_imm = WireInit(rv32_j_imm)

    val rv32_b_imm = WireInit(Cat(Fill(20, instr(31)), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)))
    val rv32_bxx_imm = WireInit(rv32_b_imm)

    val rv32_imm = WireInit(MuxLookup(cs_op2_sel, 0.U(IRIS_XLEN.W), Array(
        OP2_ITYPE -> rv32_i_imm,
        OP2_STYPE -> rv32_s_imm,
        OP2_SBTYPE -> rv32_b_imm,
        OP2_UTYPE -> rv32_u_imm,
        OP2_UJTYPE -> rv32_j_imm
    )))

    val rv16_imm = WireInit(0.U(IRIS_XLEN.W))


    io.dec_rs1idx := instr(19, 15)
    io.dec_rs2idx := instr(24, 20)
    io.dec_rdidx := instr(11, 7)

    io.dec_rs1en := cs_rs1_oen
    io.dec_rs2en := cs_rs2_oen
    io.dec_rdwen := cs_rf_wen

    io.dec_rs1x0 := (io.dec_rs1idx === 0.U(5.W))
    io.dec_rs2x0 := (io.dec_rs2idx === 0.U(5.W))

    val rv32 = WireInit(true.B) // TODO: Support 16-bit RVC Instructions

    io.dec_rv32 := rv32

    io.dec_bjp_imm := MuxLookup(cs_br_type, rv32_bxx_imm,
        Array(BR_N -> 0.U(IRIS_XLEN.W),
            BR_J -> rv32_jal_imm,
            BR_JR -> rv32_jalr_imm)) // TODO: Support 16-bit RVC Instructions



    val rv32_all0s_ilgl = WireInit(instr === 0.U(32.W))
    val rv32_all1s_ilgl = WireInit(instr === "hffffffff".U(32.W))

    val rv_all0s1s_ilgl = WireInit(rv32_all0s_ilgl | rv32_all1s_ilgl)


    val rv32_sxxi = WireInit(cs_instr_type === RV32I_SRAI || cs_instr_type === RV32I_SRLI || cs_instr_type === RV32I_SLLI)

    val rv32_sxxi_shamt_legl = WireInit(instr(25) === 0.U(1.W))
    val rv32_sxxi_shamt_ilgl = WireInit(rv32_sxxi && (!rv32_sxxi_shamt_legl))

    val rv32_dret_ilgl = WireInit((cs_instr_type === RV32I_DRET) && (!io.dbg_mode))

    val alu_op = WireInit(cs_alu_fun =/= ALU_X)

    val amoldst_op = WireInit(cs_mem_en === MEN_1) // Unimplemented Atomic memory operation // TODO: Support 16-bit RVC Instructions

    io.dec_bjp := cs_br_type =/= BR_N

    val bjp_op = WireInit(io.dec_bjp | (cs_instr_type === RV32I_MRET) | (cs_instr_type === RV32I_DRET & (!rv32_dret_ilgl)) | (cs_instr_type === RV32I_FENCE_I) | (cs_instr_type === RV32I_FENCE))

    val csr_op = WireInit(cs_wb_sel === WB_CSR)

    val muldiv_op = WireInit(0.U(Bool())) // TODO: Support RVM Instructions

    val legl_ops = WireInit(alu_op | amoldst_op | bjp_op | csr_op | muldiv_op)

    io.dec_illegl := rv_all0s1s_ilgl | rv32_sxxi_shamt_ilgl | rv32_dret_ilgl | (!legl_ops)

    val need_imm = WireInit(cs_op2_sel =/= OP2_X && cs_op2_sel =/= OP2_RS2)

    val rv32_jal = WireInit(cs_br_type === BR_J)
    val rv32_jalr = WireInit(cs_br_type === BR_JR)
    val rv32_branch = WireInit(cs_op2_sel === OP2_SBTYPE)


    // bjp_info_bus block
    val bjp_info_bus_grp = WireInit(IRIS_DECINFO_GRP_BJP)
    val bjp_info_bus_rv32 = WireInit(rv32)
    val bjp_info_bus_jump = WireInit(rv32_jal || rv32_jalr)
    val bjp_info_bus_bprdt = WireInit(io.i_predict_taken)
    val bjp_info_bus_beq = WireInit(cs_br_type === BR_EQ)
    val bjp_info_bus_bne = WireInit(cs_br_type === BR_NE)
    val bjp_info_bus_blt = WireInit(cs_br_type === BR_LT)
    val bjp_info_bus_bgt = WireInit(0.U(Bool()))
    val bjp_info_bus_bltu = WireInit(cs_br_type === BR_LTU)
    val bjp_info_bus_bgtu = WireInit(0.U(Bool()))
    val bjp_info_bus_bxx = WireInit(io.dec_bxx)
    val bjp_info_bus_mret = WireInit(cs_instr_type === RV32I_MRET)
    val bjp_info_bus_dret = WireInit(cs_instr_type === RV32I_DRET & (!rv32_dret_ilgl))
    val bjp_info_bus_fence = WireInit(cs_instr_type === RV32I_FENCE)
    val bjp_info_bus_fence_i = WireInit(cs_instr_type === RV32I_FENCE_I)

    val bjp_info_bus = Cat(bjp_info_bus_fence_i, bjp_info_bus_fence, bjp_info_bus_dret, bjp_info_bus_mret, bjp_info_bus_bxx,
        bjp_info_bus_bgtu, bjp_info_bus_bltu, bjp_info_bus_bgt, bjp_info_bus_blt, bjp_info_bus_bne, bjp_info_bus_beq,
        bjp_info_bus_bprdt, bjp_info_bus_jump, bjp_info_bus_rv32, bjp_info_bus_grp)


    // alu_info_bus_block

    val alu_info_bus_grp = WireInit(IRIS_DECINFO_GRP_ALU)
    val alu_info_bus_rv32 = WireInit(rv32)
    val alu_info_bus_add = WireInit(cs_instr_type === RV32I_ADD || cs_instr_type === RV32I_ADDI || cs_instr_type === RV32I_AUIPC)
    val alu_info_bus_sub = 0.U(Bool())
    val alu_info_bus_slt = WireInit(cs_instr_type === RV32I_SLT || cs_instr_type === RV32I_SLTI)
    val alu_info_bus_sltu = WireInit(cs_instr_type === RV32I_SLTU || cs_instr_type === RV32I_SLTIU)
    val alu_info_bus_xor = WireInit(cs_instr_type === RV32I_XOR || cs_instr_type === RV32I_XORI)
    val alu_info_bus_sll = WireInit(cs_instr_type === RV32I_SLL || cs_instr_type === RV32I_SLLI)
    val alu_info_bus_srl = WireInit(cs_instr_type === RV32I_SRL || cs_instr_type === RV32I_SRLI)
    val alu_info_bus_sra = WireInit(cs_instr_type === RV32I_SRA || cs_instr_type === RV32I_SRAI)
    val alu_info_bus_or = WireInit(cs_instr_type === RV32I_OR || cs_instr_type === RV32I_ORI)
    val alu_info_bus_and = WireInit(cs_instr_type === RV32I_AND || cs_instr_type === RV32I_ANDI)
    val alu_info_bus_lui = WireInit(cs_instr_type === RV32I_LUI)
    val alu_info_bus_op2imm = WireInit(need_imm)
    val alu_info_bus_op1pc = WireInit(cs_op1_sel === OP1_PC)
    val alu_info_bus_nop = WireInit(cs_instr_type === RV32I_ADDI && io.dec_rs1idx === 0.U && io.dec_rdidx === 0.U && !instr(31, 20).orR)
    val alu_info_bus_ecall =  WireInit(cs_instr_type === RV32I_ECALL)
    val alu_info_bus_ebreak =  WireInit(cs_instr_type === RV32I_EBREAK)
    val alu_info_bus_wfi =  WireInit(cs_instr_type === RV32I_WFI)

    val alu_info_bus = WireInit(Cat(alu_info_bus_wfi, alu_info_bus_ebreak, alu_info_bus_ecall, alu_info_bus_nop, alu_info_bus_op1pc,
        alu_info_bus_op2imm, alu_info_bus_lui, alu_info_bus_and, alu_info_bus_or, alu_info_bus_sra, alu_info_bus_srl,
        alu_info_bus_sll, alu_info_bus_xor, alu_info_bus_sltu, alu_info_bus_slt, alu_info_bus_sub, alu_info_bus_add,
        alu_info_bus_rv32, alu_info_bus_grp))


    // csr_info_bus_block

    val csr_info_bus_grp = WireInit(IRIS_DECINFO_GRP_CSR)
    val csr_info_bus_rv32 = WireInit(rv32)
    val csr_info_bus_csrrw = WireInit(cs_csr_cmd === CSR.W)
    val csr_info_bus_csrrs = WireInit(cs_csr_cmd === CSR.S)
    val csr_info_bus_csrrc = WireInit(cs_csr_cmd === CSR.C)
    val csr_info_bus_rs1imm = WireInit(cs_op1_sel === OP1_IMZ)
    val csr_info_bus_zimmm = WireInit(io.dec_rs1idx)
    val csr_info_bus_rs1is0 = WireInit(io.dec_rs1idx === 0.U)
    val csr_info_bus_csridx = WireInit(instr(31, 20))

    val csr_info_bus = WireInit(Cat(csr_info_bus_csridx, csr_info_bus_rs1is0, csr_info_bus_zimmm, csr_info_bus_rs1imm,
        csr_info_bus_csrrc, csr_info_bus_csrrs, csr_info_bus_csrrw, csr_info_bus_rv32, csr_info_bus_grp))


    // muldiv_info_bus_block
    val muldiv_info_bus = Wire(UInt(IRIS_DECINFO_MULDIV_WIDTH.W))  // Unimplemented
    muldiv_info_bus := IRIS_DECINFO_GRP_MULDIV

    // agu_info_bus_block
    val agu_info_bus = Wire(UInt(IRIS_DECINFO_AGU_WIDTH.W))  // Unimplemented
    agu_info_bus := IRIS_DECINFO_GRP_AGU

    io.dec_info := MuxCase(alu_info_bus, Array(
        alu_op -> alu_info_bus,
        amoldst_op -> agu_info_bus,
        bjp_op -> bjp_info_bus,
        csr_op -> csr_info_bus,
        muldiv_op -> muldiv_info_bus
    ))

    io.dec_imm := Mux(rv32, rv32_imm, rv16_imm)

    io.dec_mulhsu := 0.U(Bool()) // TODO: Implement Multiply and Division
    io.dec_mul := 0.U(Bool()) // TODO: Implement Multiply and Division
    io.dec_div := 0.U(Bool()) // TODO: Implement Multiply and Division
    io.dec_divu := 0.U(Bool()) // TODO: Implement Multiply and Division
    io.dec_rem := 0.U(Bool()) // TODO: Implement Multiply and Division
    io.dec_remu := 0.U(Bool()) // TODO: Implement Multiply and Division


    io.dec_jal := rv32_jal
    io.dec_jalr := rv32_jalr
    io.dec_bxx := rv32_branch

    io.dec_jalr_rs1idx := Mux(rv32, io.dec_rs1idx(IRIS_RFIDX_WIDTH-1, 0), 0.U(IRIS_RFIDX_WIDTH.W)) // TODO: Support 16-bit RVC Instructions










}


