%{
open Blaise_syntax
%}

%token EOF

%token <string> ID
%token <int> NUM
%token <string> STRING_CONST
 
%token PROGRAM FUNCTION PROCEDURE BEGIN END WHILE DO IF THEN ELSE VAR
%token CONST NOT TRUE FALSE WRITE WRITELN READ READLN RESULT
%token COLON LPAR RPAR SEMICOLON LSBRA RSBRA LCBRA RCBRA COMMA DOT
%token PLUS MINUS MULT DIV GT GTEQ LT LTEQ AND OR EQUAL MOD NEQ
%token ASSIGN

%token INTEGER STRING ARRAY REC FUN PROC BOOL

%left LOWER_THAN_ELSE
%left ELSE

%start main

%type <Blaise_syntax.program> main

%% /* Grammar rules */


main: 
  PROGRAM ID SEMICOLON decl_block begin_end_block DOT { Program($2, $4, $5) }
;
/*main:
	stmt_seq EOF{ $1 }
;*/

decl_block:
  const_block var_block decl_list { [$1;$2;Operations($3)] }
;

var_block:
  VAR decl_id_list { Vars($2)}
| { Vars([]) }
;

const_block:
  CONST decl_cid_list { Consts($2) }
| { Consts([]) }
;

decl_id_list:
  decl_id SEMICOLON { [$1] }
| decl_id SEMICOLON decl_id_list { $1::$3 }
;

decl_id:
 id_list COLON type_decl { ($3, $1) }
;
	
id_list:
  ID { [$1] }
| ID COMMA id_list { $1::$3 }
;

decl_cid_list:
  cid SEMICOLON { [$1] }
| cid SEMICOLON decl_cid_list { $1::$3 }
;

cid:
  ID EQUAL expr { ($1, $3) }
;

decl_list:
  { [] }
| decl SEMICOLON decl_list { ($1::$3) }
;

decl:
  fun_decl { $1 }
| proc_decl { $1 }
;

fun_decl:
	FUNCTION ID LPAR param_list RPAR COLON type_decl decl_block begin_end_block { Function($2, $4, $8, $9, $7) }
;

proc_decl:
	PROCEDURE ID LPAR param_list RPAR decl_block begin_end_block { Procedure($2, $4, $6, $7) }
;

param_list:
  { [] }
| param { [$1] }
| param COMMA param_list { $1::$3 }
;

param:
  ID COLON type_decl { ($1, $3) }
;

begin_end_block:
  BEGIN stmt_seq END { $2 }
;

stmt_seq:
  stmt_seq SEMICOLON stmt { Seq($1, $3) }
| stmt { $1 }
;

opt_begin_end_block:
  begin_end_block { $1 }
| stmt { $1 }
;

stmt:
  expr ASSIGN expr { Assign( $1, $3 ) }
| WHILE expr DO opt_begin_end_block { While( $2, $4 ) }
| IF expr THEN opt_begin_end_block ELSE opt_begin_end_block { If_Else( $2, $4, $6 ) }
| IF expr THEN opt_begin_end_block %prec LOWER_THAN_ELSE { If( $2, $4 ) }
| WRITE LPAR expr_list RPAR { Write( $3 ) }
| WRITELN LPAR expr_list_or_empty RPAR { WriteLn( $3 ) }
| factor LPAR expr_list_or_empty RPAR { CallProc($1, $3) }
| RESULT ASSIGN expr { Assign(Id("result"), $3 ) }
| READ LPAR id_list RPAR { Read( $3 ) }
| READLN LPAR id_list RPAR { ReadLn( $3 ) }
;

expr_list_or_empty:
	{ [] }
| expr_list { $1 } 
;

expr_list:
| expr { [$1] }
|	expr COMMA expr_list { $1::$3 }
;

expr:
  or_logic { $1 }
;

or_logic:
  and_logic { $1 }
| or_logic OR and_logic { Or($1, $3) }
;

and_logic:
  compare { $1 }
| and_logic AND compare { And($1, $3) }
;

compare:
  arith { $1 }
| compare EQUAL arith { Eq($1, $3) }
| compare NEQ arith { Neq($1, $3) }
| compare GT arith { Gt($1, $3) }
| compare LT arith { Lt($1, $3) }
| compare GTEQ arith { Gteq($1, $3) }
| compare LTEQ arith { Lteq($1, $3) }
;

arith: 
  arith PLUS term { Add($1, $3) }
| arith MINUS term { Sub($1, $3) }
| term { $1 }
;

term: 
  term MULT un_op { Mult($1, $3) }
| term DIV un_op { Div($1, $3) }
| term MOD un_op { Mod($1, $3) }
| un_op { $1 }
;

un_op:
  factor { $1 }
| NOT factor { Not($2) }
| MINUS factor { Compl($2) }
;

factor:
  NUM { Number($1) }
| STRING_CONST { String($1) }
| ID { Id($1) }
| LPAR expr RPAR { $2 }
| TRUE { Boolean(true) }
| FALSE { Boolean(false) }
| factor DOT ID { GetRecord($1, $3) }
| factor LPAR expr_list_or_empty RPAR { CallFun($1, $3) }
| factor LSBRA factor RSBRA { GetArray($1, $3) }
| LSBRA expr_list_or_empty RSBRA { Array($2) }
| LCBRA rec_decl_id_list RCBRA { Record($2) }
;

rec_decl_id_list:
	rec_decl_id { [$1] }
| rec_decl_id COMMA rec_decl_id_list { $1::$3 }
;

rec_decl_id:
	ID EQUAL expr { ($1, $3) }
;


type_decl_list:
| { [] }
| type_decl { [$1] }
| type_decl COMMA type_decl_list { $1::$3 }
;

type_decl:
  INTEGER { TNumber }
| STRING { TString }
| BOOL { TBoolean }
| func_type { $1 }
| proc_type { $1 }
| array_type { $1 }
| rec_type { $1 }
;

func_type:
	FUN LPAR type_decl_list RPAR COLON type_decl { TFun($3, $6) } 
;

proc_type:
	PROC LPAR type_decl_list RPAR { TProc($3) }
;

array_type:
  ARRAY LPAR NUM COMMA type_decl RPAR { TArray($3, $5) }
;

rec_type:
  REC LPAR rec_type_decl_id_list RPAR { TRecord($3) }
;

rec_type_decl_id_list:
	rec_type_decl_id { [$1] }
| rec_type_decl_id COMMA rec_type_decl_id_list { $1::$3 }
;

rec_type_decl_id:
	ID COLON type_decl { ($1, $3) }
;

%%