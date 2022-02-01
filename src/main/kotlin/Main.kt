import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

import antlr.*;

fun main() {
    val input = CharStream.fromStream(System.`in`);

    val lexer = BasicLexer(input);

    val tokens = CommonTokenStream(lexer);

    val parser = BasicParser(tokens);

    val tree = parser.prog();

    println(tree.toStringTree(parser));
}