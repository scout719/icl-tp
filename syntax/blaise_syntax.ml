type expr = 
	| Number of int
	| String of string
	| Boolean of bool
	| Add of expr * expr
	| Sub of expr * expr
	| Compl of expr
	| Mult of expr * expr
	| Div of expr * expr
	| Mod of expr * expr
	| Eq of expr * expr
	| Neq of expr * expr
	| Gt of expr * expr
	| Lt of expr * expr
	| Gteq of expr * expr
	| Lteq of expr * expr
	| And of expr * expr
	| Or of expr * expr
	| Not of expr
	| Id of string

type statement =
	| Assign of expr * expr
	| While of expr * statement
	| If_Else of expr * statement * statement
	| If of expr * statement
	| Write of expr list
	| WriteLn of expr list
	| Read of string list
	| ReadLn of string list
	| Seq of statement * statement

type decl_block =
	| Consts of (string * expr) list
	| Vars of string list
	| Operations of oper list

and oper = 
	| Function of string * (string list) * (decl_block list) * statement
	| Procedure of string * (string list) * (decl_block list) * statement

type program = 
	| Program of string * (decl_block list) * statement
