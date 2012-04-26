type iType =
	| TNumber
	| TString
	| TBoolean
	| TFun of (iType list) * iType
	| TProc of (iType list)
	| TArray of int * iType
	| TRecord of (string * iType) list

type expr = 
	| Number of int
	| String of string
	| Boolean of bool
	| Array of expr list
	| Record of (string * expr) list
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
	| GetArray of expr * expr
	| GetRecord of expr * string

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
	| Vars of (iType * (string list)) list
	| Operations of oper list

and oper = 
	| Function of string * ((string * iType) list) * (decl_block list) * statement * iType
	| Procedure of string * ((string * iType) list) * (decl_block list) * statement

type program = 
	| Program of string * (decl_block list) * statement
