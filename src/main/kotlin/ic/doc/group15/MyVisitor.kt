package ic.doc.group15

import ic.doc.group15.antlr.BasicParser
import ic.doc.group15.antlr.BasicParserBaseVisitor

class MyVisitor: BasicParserBaseVisitor<Void>() {

    override fun visitIfStat(ctx: BasicParser.IfStatContext?): Void? {
        println("we're in an if-statement")
        if (ctx != null) {
            println(ctx.children)
        }
        return visitChildren(ctx)
    }

    override fun visitSkipStat(ctx: BasicParser.SkipStatContext?): Void? {
        println("we're in a skip-statement")
        return visitChildren(ctx)
    }

    override fun visitFunc(ctx: BasicParser.FuncContext?): Void? {
        println("we're in a function")
        if (ctx != null) {
            println(ctx.ident().IDENT())
        }
        return visitChildren(ctx)
    }

}