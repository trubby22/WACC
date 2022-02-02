parser grammar BasicParser;

options {
  tokenVocab=BasicLexer;
}

// EOF indicates that the program must consume to the end of the input.
prog: (expr)* EOF;

program: BEGIN stat END EOF;

func: type ident OPEN_PARENTHESES param_list? CLOSE_PARENTHESES IS stat END;

param_list: param (COMMA param)*;

param: type ident;

stat: SKIP_STAT
| type ident ASSIGN assign_rhs
| assign_lhs ASSIGN assign_rhs
| READ assign_lhs
| FREE expr
| RETURN expr
| EXIT expr
| PRINT expr
| PRINTLN expr
| IF expr THEN stat ELSE stat FI
| WHILE expr DO stat DONE
| BEGIN stat END
| stat END_STAT stat
;

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

type: base_type #BaseType
| type OPEN_BRACKETS CLOSE_BRACKETS #ArrayType
| pair_type #PairType
;

base_type: T_INT | T_BOOL | T_CHAR | T_STRING;

pair_type: PAIR OPEN_PARENTHESES pair_elem_type COMMA pair_elem_type CLOSE_PARENTHESES;

pair_elem_type: base_type
| ArrayType
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

char_liter: APOSTROPHE CHAR APOSTROPHE | BACKSLASH ESC_CHAR;

str_liter: DOUBLE_QUOTE CHAR* DOUBLE_QUOTE;

array_liter: OPEN_BRACKETS (expr (COMMA expr)*)? CLOSE_BRACKETS;

pair_liter: NULL;
