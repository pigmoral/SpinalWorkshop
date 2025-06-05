package workshop.pwm

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._

// APB configuration class (generic/parameter)
case class ApbConfig(addressWidth : Int,
                     dataWidth    : Int,
                     selWidth     : Int)

// APB interface definition
case class Apb(config: ApbConfig) extends Bundle with IMasterSlave {
  // Master Drive
  val PSEL = Bits(config.selWidth bits)
  val PENABLE = Bool()
  val PWRITE = Bool()
  val PADDR = UInt(config.addressWidth bits)
  val PWDATA = Bits(config.dataWidth bits)

  // Slave Drive
  val PRDATA = Bits(config.dataWidth bits)
  val PREADY = Bool()

  override def asMaster(): Unit = {
    out(PSEL, PENABLE, PWRITE, PADDR, PWDATA)
    in(PRDATA, PREADY)
  }
}

case class ApbPwm(apbConfig: ApbConfig, timerWidth: Int) extends Component {
  require(apbConfig.dataWidth == 32)
  require(apbConfig.selWidth == 1)

  val io = new Bundle{
    val apb = slave(Apb(apbConfig))
    val pwm = out(Bool())
  }

  val logic = new Area {
    val enable    = Reg(Bool()) init(False)
    val timer     = Reg(UInt(timerWidth bits)) init(0)
    val dutyCycle = Reg(UInt(timerWidth bits)) init(0)
    val output    = Reg(Bool()) init(False)

    when(enable === True) {
      timer := timer + 1
    }

    when(timer === 0) {
      output := True
    }
    when(timer === dutyCycle) {
      output := False
    }

    io.pwm := output
  }

  val control = new Area {
    val doWrite = io.apb.PSEL(0) && io.apb.PENABLE && io.apb.PWRITE

    io.apb.PRDATA := 0
    io.apb.PREADY := True

    switch(io.apb.PADDR) {
      is(0) {
        io.apb.PRDATA(0) := logic.enable
        when(doWrite) {
          logic.enable := io.apb.PWDATA(0)
        }
      }
      is(4) {
        io.apb.PRDATA := logic.dutyCycle.asBits.resized
        when(doWrite) {
          logic.dutyCycle := io.apb.PWDATA.asUInt.resized
        }
      }
    }
  }

//  val control = new Area {
//    io.apb.PRDATA := 0
//    io.apb.PREADY := True
//
//    when(io.apb.PSEL(0) && io.apb.PENABLE) {
//      when(io.apb.PWRITE) {
//        // Write
//        switch(io.apb.PADDR) {
//          is(0) { logic.enable := io.apb.PWDATA(0) }
//          is(4) { logic.dutyCycle := io.apb.PWDATA.asUInt.resized }
//        }
//      } otherwise {
//        // Read
//        io.apb.PRDATA := B"32'00000000"
//        switch(io.apb.PADDR) {
//          is(0) { io.apb.PRDATA(0) := logic.enable }
//          is(4) { io.apb.PRDATA := logic.dutyCycle.asBits.resized }
//        }
//      }
//    }
//  }
}