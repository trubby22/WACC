parser grammar WaccParser;

options {
  tokenVocab=WaccLexer;
}

func: type ident OPEN_PARENTHESES (param (COMMA param)*)? CLOSE_PARENTHESES IS
          (stat END_STAT)?
          valid_return_stat
      END;

param: type ident;

stat: SKIP_STAT                                 #skipStat
    | type ident ASSIGN assign_rhs              #declarationStat
    | assign_lhs ASSIGN assign_rhs              #assignmentStat
    | READ assign_lhs                           #readStat
    | FREE expr                                 #freeStat
    | EXIT expr                                 #exitStat
    | PRINT expr                                #printStat
    | PRINTLN expr                              #printlnStat
    | IF expr THEN stat ELSE stat FI            #ifStat
    | WHILE expr DO stat DONE                   #whileStat
    | BEGIN stat END                            #beginEndStat
    | stat END_STAT stat                        #sequenceStat
;

return_stat: (stat END_STAT)? (RETURN | EXIT) expr;

// Needed for identifying return statements inside blocks
valid_return_stat: return_stat
                 | WHILE expr DO valid_return_stat DONE
                 | IF expr THEN valid_return_stat ELSE valid_return_stat FI
                 | BEGIN valid_return_stat END
;

assign_lhs: ident                               #identAssign
          | array_elem                          #arrayAssign
          | pair_elem                           #pairAssign
;

assign_rhs: expr                                                          #exprAssign
          | array_liter                                                   #arrayLiterAssign
          | NEWPAIR OPEN_PARENTHESES expr COMMA expr CLOSE_PARENTHESES    #newPairAssign
          | pair_elem                                                     #pairElemAssign
          | CALL ident OPEN_PARENTHESES arg_list? CLOSE_PARENTHESES       #callAssign
;

arg_list: expr (COMMA expr)*;

pair_elem: FST expr                             #fstPair
         | SND expr                             #sndPair
;

type: base_type                                 #baseType
    | array_type                                #arrayType
    | pair_type                                 #pairType
;

base_type: T_INT | T_BOOL | T_CHAR | T_STRING;

array_type: (base_type | pair_type) (OPEN_BRACKETS CLOSE_BRACKETS)+;

pair_type: PAIR OPEN_PARENTHESES pair_elem_type COMMA pair_elem_type CLOSE_PARENTHESES;

pair_elem_type: base_type
              | array_type
              | PAIR
;

expr: int_liter                                 #intLiterExpr
    | bool_liter                                #boolLiterExpr
    | char_liter                                #charLiterExpr
    | str_liter                                 #strLiterExpr
    | pair_liter                                #pairLiterExpr
    | ident                                     #identExpr
    | array_elem                                #arrayElemExpr
    | unary_op expr                             #unaryOpExpr
    | expr binary_op expr                       #binaryOpExpr
    | OPEN_PARENTHESES expr CLOSE_PARENTHESES   #bracketExpr
;

unary_op: BANG #bangUnaryOp
| MINUS        #minusUnaryOp
| LEN          #lenUnaryOp
| ORD          #ordUnaryOp
| CHR          #chrUnaryOp
;

binary_op: MULT #multBinaryOp
| DIV           #divBinaryOp
| MOD           #modBinaryOp
| PLUS          #plusBinaryOp
| MINUS         #minusBinaryOp
| GT            #gtBinaryOp
| GTE           #gteBinaryOp
| LT            #ltBinaryOp
| LTE           #lteBinaryOp
| EQUALS        #equalsBinaryOp
| NOT_EQUALS    #notEqualsBinaryOp
| AND           #andBinaryOp
| OR            #orBinaryOp
;

ident: IDENT;

array_elem: ident (OPEN_BRACKETS expr CLOSE_BRACKETS)+;

int_liter: int_sign? INTEGER;

int_sign: PLUS | MINUS;

bool_liter: TRUE   #tBool
| FALSE            #fBool
 ;

char_liter: CHAR_LITER_TOKEN | ESC_CHAR_LITER;

str_liter: STRING_LITER_TOKEN;

array_liter: OPEN_BRACKETS (expr (COMMA expr)*)? CLOSE_BRACKETS;

pair_liter: NULL;

// EOF indicates that the program must consume to the end of the input.
program: BEGIN (func)* stat END EOF;
