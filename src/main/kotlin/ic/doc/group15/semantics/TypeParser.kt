package ic.doc.group15.semantics

import ic.doc.group15.antlr.WaccParser.*
import ic.doc.group15.semantics.ast.TypeError

class TypeParser {

    companion object {

        fun parse(symbolTable: SymbolTable, ctx: TypeContext): Type {
            return when {
                ctx.base_type() != null -> {
                    parse(symbolTable, ctx.base_type())
                }
                ctx.pair_type() != null -> {
                    parse(symbolTable, ctx.pair_type())
                }
                else -> {
                    parse(symbolTable, ctx.array_type())
                }
            }
        }

        private fun parse(symbolTable: SymbolTable, ctx: Base_typeContext): Type {
            val result = symbolTable.lookupAll(ctx.text)
                ?: throw TypeError(
                    "${ctx.text} is not a valid type"
                )
            assert(result is Type)
            return (result as Type)
        }

        private fun parse(symbolTable: SymbolTable, ctx: Pair_typeContext): Type {
            return PairType(
                parse(symbolTable, ctx.pair_elem_type(0)),
                parse(symbolTable, ctx.pair_elem_type(1))
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
                    symbolTable.lookupAll(ctx.text) as PairType
                }
            }
        }

        private fun parse(symbolTable: SymbolTable, ctx: Array_typeContext): Type {
            val type: Type
            if (ctx.pair_type() != null) {
                type = parse(symbolTable, ctx.pair_type() as Pair_typeContext)
            } else {
                assert(ctx.base_type() != null)
                type = parse(symbolTable, ctx.base_type() as Base_typeContext)
            }
            return ArrayType(type, ctx.array_brackets().size)
        }
    }
}
