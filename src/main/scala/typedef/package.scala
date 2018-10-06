
package typedef

import chisel3._
import chisel3.util._

import scala.math._

object Constants extends
        TypeDef.RISCVConstants with
        IrisProcConstants with
        ScalarOpConstants with
        MemoryOpConstants with
        Rv32iInstrConstants with
        Decode_Revelent_Macro
{

}
