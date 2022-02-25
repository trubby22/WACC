package ic.doc.group15.codegeneration

abstract class Instr

class LDRimmInt(val regNum: Int, val loadVal: Int) : Instr()
class LDRimmString(val regNum: Int, val loadVal: String) : Instr()
class LDRsp(val regNum: Int, val spOffset: Int) : Instr()
class LDRSBsp(val regNum: Int, val spOffset: Int) : Instr()
class MOVimmBool(val regNum : Int, val movVal : Boolean) : Instr()
class MOVimmChar(val regNum : Int, val movVal : Char) : Instr()
class STRsp(val regNum: Int, val spPos: Int) : Instr()
class STRBsp(val regNum: Int, val spPos: Int) : Instr()
class ADDS(val reg1: Int, val reg2: Int, val re3: Int) : Instr()
class AND(val reg1: Int, val reg2: Int, val re3: Int) : Instr()
class SMULL(val reg1: Int, val reg2: Int, val reg3: Int, val reg4: Int) : Instr()
class CMP(val reg1: Int, val reg2: Int) : Instr()
class MOVEQ(val regNum: Int, val value: Boolean) : Instr()
class MOVNE(val regNum: Int, val value: Boolean) : Instr()
