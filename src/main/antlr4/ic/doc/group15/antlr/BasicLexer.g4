lexer grammar BasicLexer;

//whitespace
WS: [ \n\t\r]+ -> skip;

//comment
COMMENT: '#' ~('\n')* '\n' -> skip;

//types
T_INT: 'int';
T_BOOL: 'bool';
T_CHAR: 'char';
T_STRING: 'string';

//boolean values
TRUE: 'true';
FALSE: 'false';

//unary operators
BANG: '!';
LEN: 'len';
ORD: 'ord';
CHR: 'chr';

//function prototyping
IS: 'is';

//functions
READ: 'read';
FREE: 'free';
RETURN: 'return';
EXIT: 'exit';
PRINT: 'print';
PRINTLN: 'println';

//flow control operators
IF: 'if';
FI: 'fi';
THEN: 'then';
ELSE: 'else';
WHILE: 'while';
DO: 'do';
DONE: 'done';
SKIP_STAT: 'skip';
END_STAT: ';';
BEGIN: 'begin';
END: 'end';

//memory assignments
NEWPAIR: 'newpair';
CALL: 'call';
NULL: 'null';

//pair
PAIR: 'pair';
FST: 'fst';
SND: 'snd';

//binary operators
ASSIGN: '=';
MULT: '*';
DIV: '/';
MOD: '%';
PLUS: '+';
MINUS: '-';
GT: '>';
GTE: '>=';
LT: '<';
LTE: '<=';
EQUALS: '==';
NOT_EQUALS: '!=';
AND: '&&';
OR: '||';

//brackets
OPEN_PARENTHESES: '(';
OPEN_BRACKETS: '[';
COMMA: ',';
CLOSE_PARENTHESES: ')';
CLOSE_BRACKETS: ']';

//numbers
fragment DIGIT: [0-9];
fragment ALPHABET: [a-zA-Z];
fragment ALPHANUMERIC: (DIGIT | ALPHABET);
INTEGER: DIGIT+;

//identifiers
IDENT: ('_' | ALPHABET) ('_' | ALPHANUMERIC)*;

//characters
BACKSLASH: '\\';
APOSTROPHE: '\'';
ESC_CHAR: '0' | 'b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\';
CHAR: ~('\\' | '\'' | '"');
//CHAR: ALPHABET;
CHAR_LITER_TOKEN: '\'' CHAR '\'';
ESC_CHAR_LITER: '\'' '\\' ESC_CHAR '\'';
STRING_LITER_TOKEN: '"' (CHAR | '\\' ESC_CHAR)* '"';
DOUBLE_QUOTE: '"';
