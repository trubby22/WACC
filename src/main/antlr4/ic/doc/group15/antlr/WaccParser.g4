parser grammar WaccParser;

options {
  tokenVocab=WaccLexer;
}

// EOF indicates that the program must consume to the end of the input.
program: BEGIN (func)* stat END EOF;

func: type ident OPEN_PAREN (param (COMMA param)*)? CLOSE_PAREN IS
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
    | return_stat                               #returnStat
    | stat END_STAT stat                        #sequenceStat
;

return_stat: (RETURN | EXIT) expr;

// Needed for identifying return statements inside blocks
valid_return_stat: (stat END_STAT)? return_stat
                 | WHILE expr DO valid_return_stat DONE
                 | IF expr THEN valid_return_stat ELSE valid_return_stat FI
                 | BEGIN valid_return_stat END
;

assign_lhs: ident                               #identAssignLhs
          | array_elem                          #arrayElemAssignLhs
          | pair_elem                           #pairElemAssignLhs
;

assign_rhs: expr                                              #exprAssignRhs
          | array_liter                                       #arrayLiterAssignRhs
          | NEWPAIR OPEN_PAREN expr COMMA expr CLOSE_PAREN    #newPairAssignRhs
          | pair_elem                                         #pairElemAssignRhs
          | CALL ident OPEN_PAREN arg_list? CLOSE_PAREN       #callAssignRhs
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

pair_type: PAIR OPEN_PAREN pair_elem_type COMMA pair_elem_type CLOSE_PAREN;

pair_elem_type: base_type
              | array_type
              | PAIR
;

expr: OPEN_PAREN expr CLOSE_PAREN               #bracketExpr
    | int_liter                                 #singleElemExpr
    | bool_liter                                #singleElemExpr
    | char_liter                                #singleElemExpr
    | str_liter                                 #singleElemExpr
    | pair_liter                                #singleElemExpr
    | array_elem                                #singleElemExpr
    | ident                                     #identExpr
    | BANG expr                                 #unaryExpr
    | MINUS expr                                #unaryExpr
    | LEN expr                                  #unaryExpr
    | ORD expr                                  #unaryExpr
    | CHR expr                                  #unaryExpr
    | expr MULT expr                            #binaryExpr
    | expr DIV expr                             #binaryExpr
    | expr MOD expr                             #binaryExpr
    | expr PLUS expr                            #binaryExpr
    | expr MINUS expr                           #binaryExpr
    | expr GT expr                              #binaryExpr
    | expr GTE expr                             #binaryExpr
    | expr LT expr                              #binaryExpr
    | expr LTE expr                             #binaryExpr
    | expr EQUALS expr                          #binaryExpr
    | expr NOT_EQUALS expr                      #binaryExpr
    | expr AND expr                             #binaryExpr
    | expr OR expr                              #binaryExpr
;

ident: IDENT;

array_elem: ident (OPEN_BRACKETS expr CLOSE_BRACKETS)+;

int_liter: int_liter_negative | int_liter_positive;

int_liter_negative: MINUS NEGATIVE_INTEGER;
int_liter_positive: PLUS? POSITIVE_INTEGER;

bool_liter: TRUE                                #tBool
          | FALSE                               #fBool
;

char_liter: CHAR_LITER_TOKEN | ESC_CHAR_LITER;

str_liter: STRING_LITER_TOKEN;

array_liter: OPEN_BRACKETS (expr (COMMA expr)*)? CLOSE_BRACKETS;

pair_liter: NULL;
