
package pipeline

import chisel3._
import chisel3.util._

object CSR
{
    // commands
    val SZ = 3.W
    val X = 0.asUInt(SZ)
    val N = 0.asUInt(SZ)
    val W = 1.asUInt(SZ)
    val S = 2.asUInt(SZ)
    val C = 3.asUInt(SZ)
    val I = 4.asUInt(SZ)
    val R = 5.asUInt(SZ)

    val ADDRSZ = 12
    val firstCtr = CSRs.cycle
    val firstCtrH = CSRs.cycleh
    val firstHPC = CSRs.hpmcounter3
    val firstHPCH = CSRs.hpmcounter3h
    //val firstHPE = CSRs.mhpmevent3
    val firstMHPC = CSRs.mhpmcounter3
    val firstMHPCH = CSRs.mhpmcounter3h
    val firstHPM = 3
    val nCtr = 32
    val nHPM = nCtr - firstHPM
    val hpmWidth = 40
}
