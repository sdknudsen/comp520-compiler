%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID

%token PLUS MINUS TIMES DIV
%token EOF

%token VAR ASSIGNMENT
%token T_INT T_FLOAT T_STRING

%token PRINT READ
%token COLON SEMICOLON
%token LPAREN RPAREN

%token IF THEN ELSE ENDIF
%token WHILE DO DONE

%left PLUS MINUS
%left TIMES DIV 
%nonassoc NEG

%start <Ast.ast> program

%%

program:
  | declaration* statement* EOF { Prog($1,$2) }

declaration:
  | VAR ID COLON decl_type SEMICOLON { Dec($2,$4) }

decl_type:
  | T_INT    { TInt }
  | T_FLOAT  { TFloat }
  | T_STRING { TString }

statement:
  | ID ASSIGNMENT expression SEMICOLON  { Assign($1,$3) }
  | PRINT expression SEMICOLON    { Print($2) }
  | READ ID SEMICOLON   { Read($2) }
  | IF expression THEN statement* ELSE statement* ENDIF { Ifte($2,$4,$6) }
  | IF expression THEN statement* ENDIF  { Ift($2,$4) }
  | WHILE expression DO statement* DONE  { While($2,$4) }
  | error       { Error.print_error $startpos "syntax error in statement" }

expression:
  | INT     { ILit($1) }
  | FLOAT     { FLit($1) }
  | STRING    { SLit($1) }
  | ID      { Var($1) }
  | LPAREN expression RPAREN  { $2 }
  | expression PLUS expression  { Bexp(PLUS,$1,$3) }
  | expression MINUS expression { Bexp(MINUS,$1,$3) }
  | expression TIMES expression { Bexp(TIMES,$1,$3) }
  | expression DIV expression   { Bexp(DIV,$1,$3) }
  | MINUS expression %prec NEG  { Uexp(NEG,$2) }
  | error     { Error.print_error $startpos "syntax error in expression" }
