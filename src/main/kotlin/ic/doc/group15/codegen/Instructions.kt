package ic.doc.group15.codegeneration

abstract class Instr

class LDRimmInt(val regNum: Int, val loadVal: Int) : Instr()
class LDRimmString(val regNum: Int, val loadVal: String) : Instr()
class MOVimmBool(val regNum : Int, val movVal : Boolean) : Instr()
class MOVimmChar(val regNum : Int, val movVal : Char) : Instr()
class STRsp(val regNum: Int, val spPos: Int) : Instr()
class STRBsp(val regNum: Int, val spPos: Int) : Instr()

