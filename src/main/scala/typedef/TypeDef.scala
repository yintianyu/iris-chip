
package typedef
package TypeDef
{
    import chisel3._
    import chisel3.util._

    trait RISCVConstants
    {
        val IRIS_INSTR_SIZE = 32
        val IRIS_PC_SIZE = 32
        val IRIS_RFIDX_WIDTH = 5
        val IRIS_DECINFO_WIDTH = 32
        val IRIS_XLEN = 32
    }
}

