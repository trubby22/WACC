package ic.doc.group15.ssa

import ic.doc.group15.ast.BinaryOp

// Quadruple form - three address code maintains an unlimited amount of registers,
// representing either declared variables or temporaries
sealed interface ThreeAddressCode

// Assignments
data class AssignBinOp(val reg: Register, val op: BinaryOp, val x: Operand, val y: Operand): ThreeAddressCode
data class AssignValue(val reg: Register, val x: Operand): ThreeAddressCode
data class AssignCall(val reg: Register, val f: Func, val args: Collection<Operand>): ThreeAddressCode

// (Void) function calls
data class Call(val f: Func, val args: Collection<Operand>): ThreeAddressCode

// Branch statements
data class BranchIf(val cond: Operand, val label: Label): ThreeAddressCode
data class Branch(val label: Label): ThreeAddressCode

// Memory instructions (used for heap allocations/value storing)
data class Allocate(val reg: Register, val amount: Operand): ThreeAddressCode
data class Load(val reg: Register, val x: Operand): ThreeAddressCode
data class Store(val reg: Register, val x: Operand): ThreeAddressCode
