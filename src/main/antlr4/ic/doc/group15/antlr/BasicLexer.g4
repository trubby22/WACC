lexer grammar BasicLexer;

//operators
PLUS: '+' ;
MINUS: '-' ;

//brackets
OPEN_PARENTHESES: '(' ;
CLOSE_PARENTHESES: ')' ;

//numbers
fragment DIGIT: '0'..'9' ;

INTEGER: DIGIT+ ;

//skip
SKIP_STAT: 'skip' ;

BEGIN: 'begin' ;
END: 'end' ;

//whitespace
WS: [ \n\t\r]+ -> skip;

//comment
COMMENT: '#' ~('\n')* '\n' -> skip ;