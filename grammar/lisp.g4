grammar lisp;

options{language=Python3;}

program: form* EOF;

form: sexpr;

sexpr: atom | list;

atom: NUMBER
    | STRING
    | SYMBOL
    | QUOTE sexpr
    | NIL
    | TRUE
    ;

list: '(' sexpr* ')';

SYMBOL: [a-zA-Z_!$%&*+\-./:<=>?@^~][a-zA-Z0-9_!$%&*+\-./:<=>?@^~]*;
NUMBER: '-'? [0-9]+ ('.' [0-9]+)?;
STRING: '"' (~["\r\n] | '\\' .)* '"';
QUOTE: '\'';
NIL: 'nil';
TRUE: 't';
COMMENT: ';' ~[\r\n]* -> skip;
WS: [ \t\r\n]+ -> skip;