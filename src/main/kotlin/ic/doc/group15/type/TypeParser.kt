package ic.doc.group15.type

import ic.doc.group15.SymbolTable
import ic.doc.group15.antlr.WaccParser.*

class TypeParser {

    companion object {

        fun returnTypeIsVoid(ctx: Return_typeContext): Boolean = ctx.T_VOID() != null

        fun parse(symbolTable: SymbolTable, ctx: Return_typeContext): Type {
            return when {
                ctx.type() != null -> {
                    parse(symbolTable, ctx.type())
                }
                else -> {
                    ReturnableType.VOID
                }
            }
        }

        fun parse(symbolTable: SymbolTable, ctx: TypeContext): Type {
            return when {
                ctx.base_type() != null -> {
                    parse(symbolTable, ctx.base_type())
                }
                ctx.pair_type() != null -> {
                    parse(symbolTable, ctx.pair_type())
                }
                ctx.array_type() != null -> {
                    parse(symbolTable, ctx.array_type())
                }
                else -> {
                    assert(ctx.pointer_type() != null)
                    parse(symbolTable, ctx.pointer_type())
                }
            }
        }

        private fun parse(symbolTable: SymbolTable, ctx: Base_typeContext): Type {
            val result = symbolTable.lookupAll(ctx.text)
            assert(result is Type)
            return result as Type
        }

        private fun parse(symbolTable: SymbolTable, ctx: Pair_typeContext): Type {

            return PairType(
                parse(symbolTable, ctx.pair_elem_type(0)) as VariableType,
                parse(symbolTable, ctx.pair_elem_type(1)) as VariableType
            )
        }

        private fun parse(symbolTable: SymbolTable, ctx: Pair_elem_typeContext): Type {
            return when {
                ctx.base_type() != null -> {
                    parse(symbolTable, ctx.base_type())
                }
                ctx.array_type() != null -> {
                    parse(symbolTable, ctx.array_type())
                }
                else -> {
                    // Last resort is type erased pair
                    symbolTable.lookupAll(ctx.text)
                }
            } as Type
        }

        private fun parse(symbolTable: SymbolTable, ctx: Array_typeContext): Type {
            val type: Type = if (ctx.pair_type() != null) {
                parse(symbolTable, ctx.pair_type() as Pair_typeContext)
            } else {
                assert(ctx.base_type() != null)
                parse(symbolTable, ctx.base_type() as Base_typeContext)
            }
            return ArrayType(type as VariableType, ctx.array_brackets().size)
        }

        private fun parse(symbolTable: SymbolTable, ctx: Pointer_typeContext): Type {
            val type: Type = parse(symbolTable, ctx.type())
            return PointerType(type as VariableType, 1)
        }
    }
}
