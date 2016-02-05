%{
open Ast
open Error
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING

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

%start main
%type <Ast.ast> main
%%

main:	  declaration* stmt* EOF	{ Prog($1,$2) }
;

declaration:
	  VAR ID COLON typ SEMICOLON	{ Dec($2,$4) }
;

typ:	  T_INT 			{ TInt }
	| T_FLOAT			{ TFloat }
	| T_STRING			{ TString }
;

stmt:	  ID ASSIGNMENT expr SEMICOLON 	{ Assign($1,$3) }
	| PRINT expr SEMICOLON		{ Print($2) }
	| READ ID SEMICOLON		{ Read($2) }

	| IF expr THEN stmt* ELSE stmt* ENDIF { Ifte($2,$4,$6) }
	| IF expr THEN stmt* ENDIF 	{ Ift($2,$4) }
	| WHILE expr DO stmt* DONE 	{ While($2,$4) }
    	| error				{ raise (ParseError($startpos,"statement")) }
;

expr:	  INT			{ ILit($1) }
	| FLOAT			{ FLit($1) }
	| STRING		{ SLit($1) }
	| ID			{ Var($1) }

	| LPAREN expr RPAREN	{ $2 }
	| expr PLUS expr	{ Bexp(PLUS,$1,$3) }
	| expr MINUS expr	{ Bexp(MINUS,$1,$3) }
	| expr TIMES expr	{ Bexp(TIMES,$1,$3) }
	| expr DIV expr		{ Bexp(DIV,$1,$3) }
	| MINUS expr %prec NEG	{ Uexp(NEG,$2) }
    	| error			{ raise (ParseError($startpos,"expression")) }
