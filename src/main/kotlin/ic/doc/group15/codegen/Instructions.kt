package ic.doc.group15.codegeneration

abstract class Line

abstract class Instr : Line() {
    private var label : String = ""

    fun setLabel(label : String) {
        this.label = label
    }
}

class LDRimmInt(val reg: Int, val loadVal: Int) : Instr() // LDR r4, =5
class LDRimmString(val reg: Int, val loadVal: String) : Instr() // LDR r4, =msg_0
class LDRsp(val reg: Int, val spOffset: Int) : Instr() // LDR r4, [sp, #4]
class LDRSBsp(val reg: Int, val spOffset: Int) : Instr() // LDRSB r4, [sp, #1]
class MOVimmBool(val reg : Int, val movVal : Boolean) : Instr() // MOV r4, #1
class MOVimmChar(val reg : Int, val movVal : Char) : Instr() // e.g. MOV r4, #'c'
class MOVreg(val reg1 : Int, val reg2: Int) : Instr()
class MOVEQ(val reg: Int, val value: Boolean) : Instr()
class MOVNE(val reg: Int, val value: Boolean) : Instr()
class STRsp(val reg: Int, val spPos: Int) : Instr() // STR r4, [sp]
class STRBsp(val reg: Int, val spPos: Int) : Instr()
class ADDS(val reg1: Int, val reg2: Int, val re3: Int) : Instr()
class ADD(val reg1: Int, val reg2: Int, val value: Int) : Instr()
class ADDspImm(val reg1: Int, val value: Int) : Instr() // stack pointer is always the second argument e.g. this translates to the assembly: ADD reg1, sp, value
class ADDspSpImm(val value : Int) : Instr() // only for incrementing the sp e.g. ADD sp, sp, #4
class SUBspSpImm(val value : Int) : Instr() // only for decrementing the sp e.g. SUB sp, sp, #4
class AND(val reg1: Int, val reg2: Int, val re3: Int) : Instr()
class SMULL(val reg1: Int, val reg2: Int, val reg3: Int, val reg4: Int) : Instr()
class CMPreg(val reg1: Int, val reg2: Int) : Instr()
class CMPimm(val reg1: Int, val value: Int) : Instr()
class BEQ(val label : String) : Instr()
class B(val label : String) : Instr()
class BL(val label : String) : Instr()
class PUSHlr : Instr() // so far i have only seen PUSH used to push lr so for now PUSH is hardcoded to only push lr
class POPpc : Instr() // so far i have only seen POP used to pop pc so for now POP is hardcoded to only pop pc

class Label(val label: String) : Line()