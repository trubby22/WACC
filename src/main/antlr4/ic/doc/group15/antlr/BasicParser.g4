parser grammar BasicParser;

options {
  tokenVocab=BasicLexer;
}

func: type ident OPEN_PARENTHESES param_list? CLOSE_PARENTHESES IS (stat
END_STAT)? valid_return_stat END;

param_list: param (COMMA param)*;

param: type ident;

stat: SKIP_STAT                    #skipStat
| type ident ASSIGN assign_rhs     #declarationStat
| assign_lhs ASSIGN assign_rhs     #assignmentStat
| READ assign_lhs                  #readStat
| FREE expr                        #freeStat
| EXIT expr                        #exitStat
| PRINT expr                       #printStat
| PRINTLN expr                     #printlnStat
| IF expr THEN stat ELSE stat FI   #ifStat
| WHILE expr DO stat DONE          #whileStat
| BEGIN stat END                   #beginEndStat
| RETURN expr                      #returnStat
| stat END_STAT stat               #sequenceStat
;

valid_return_stat: return_stat | while_return | if_return | begin_end_return;

return_stat: (stat END_STAT)? (RETURN | EXIT) expr;

while_return: WHILE expr DO valid_return_stat DONE;
if_return: IF expr THEN valid_return_stat ELSE valid_return_stat FI;
begin_end_return: BEGIN valid_return_stat END;


assign_lhs: ident
| array_elem
| pair_elem
;

assign_rhs: expr
| array_liter
| NEWPAIR OPEN_PARENTHESES expr COMMA expr CLOSE_PARENTHESES
| pair_elem
| CALL ident OPEN_PARENTHESES arg_list? CLOSE_PARENTHESES
;

arg_list: expr (COMMA expr)*;

pair_elem: FST expr
| SND expr
;

type: base_type   #baseType
| array_type      #arrayType
| pair_type       #pairType
;

base_type: T_INT | T_BOOL | T_CHAR | T_STRING;

array_type: (base_type | pair_type) (OPEN_BRACKETS CLOSE_BRACKETS)+ ;

pair_type: PAIR OPEN_PARENTHESES pair_elem_type COMMA pair_elem_type CLOSE_PARENTHESES;

pair_elem_type: base_type
| array_type
| PAIR
;

expr: int_liter
| bool_liter
| char_liter
| str_liter
| pair_liter
| ident
| array_elem
| unary_op expr
| expr binary_op expr
| OPEN_PARENTHESES expr CLOSE_PARENTHESES
;

unary_op: BANG | MINUS | LEN | ORD | CHR;

binary_op: MULT | DIV | MOD | PLUS | MINUS | GT | GTE | LT | LTE | EQUALS | NOT_EQUALS | AND | OR;

ident: IDENT;

array_elem: ident (OPEN_BRACKETS expr CLOSE_BRACKETS)+;

int_liter: int_sign? INTEGER;

int_sign: PLUS | MINUS;

bool_liter: TRUE | FALSE;

char_liter: CHAR_LITER_TOKEN | ESC_CHAR_LITER;

str_liter: STRING_LITER_TOKEN;

array_liter: OPEN_BRACKETS (expr (COMMA expr)*)? CLOSE_BRACKETS;

pair_liter: NULL;

// EOF indicates that the program must consume to the end of the input.
program: BEGIN (func)* stat END EOF;

//test_prog: APOSTROPHE EOF ;