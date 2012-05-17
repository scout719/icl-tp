open Blaise_iType;;

type expr = 
	| Number of int
	| String of string
	| Boolean of bool
	| Array of expr list * iType
	| Record of (string * expr) list * iType
	| Add of expr * expr * iType
	| Sub of expr * expr * iType
	| Compl of expr * iType
	| Mult of expr * expr * iType
	| Div of expr * expr * iType
	| Mod of expr * expr * iType
	| Eq of expr * expr * iType
	| Neq of expr * expr * iType
	| Gt of expr * expr * iType
	| Lt of expr * expr * iType
	| Gteq of expr * expr * iType
	| Lteq of expr * expr * iType
	| And of expr * expr * iType
	| Or of expr * expr * iType
	| Not of expr * iType
	| Id of string * iType
	| GetArray of expr * expr * iType
	| GetRecord of expr * string * iType
	| CallFun of expr * (expr list) * iType;;

type statement =
	| Assign of expr * expr * iType
	| While of expr * statement * iType
	| If_Else of expr * statement * statement *iType
	| If of expr * statement * iType
	| Write of expr list * iType
	| WriteLn of expr list * iType
	| Read of string list * (iType list) * iType
	| ReadLn of string list * (iType list) * iType
	| Seq of statement * statement * iType
	| CallProc of expr * (expr list) * iType;;

type decl_block =
	| Consts of (string * expr) list * iType
	| Vars of (iType * (string list)) list * iType
	| Operations of oper list * iType

and oper = 
	| Function of string * ((string * iType) list) * (decl_block list) * statement * iType
	| Procedure of string * ((string * iType) list) * (decl_block list) * statement * iType;;

type program = 
	| Program of string * (decl_block list) * statement * iType;;

let get_type e =
	match e with
		| Number _ -> TNumber
		| String _ -> TString
		| Boolean _ -> TBoolean
		| Array(_,t) -> t 
		| Record(_,t) -> t
		| Add(_,_,t) -> t
		| Sub(_,_,t) -> t
		| Compl(_,t) -> t
		| Mult(_,_,t) -> t
		| Div(_,_,t) -> t
		| Mod(_,_,t) -> t
		| Eq(_,_,t) -> t
		| Neq(_,_,t) -> t
		| Gt(_,_,t) -> t
		| Lt(_,_,t) -> t
		| Gteq(_,_,t) -> t
		| Lteq(_,_,t) -> t
		| And(_,_,t) -> t
		| Or(_,_,t) -> t
		| Not(_,t) -> t
		| Id(_,t) -> t
		| GetArray(_,_,t) -> t
		| GetRecord(_,_,t) -> t
		| CallFun(_,_,t) -> t

let get_type_stat s =
	match s with
		| Assign (_, _, t) -> t
		| While (_, _, t) -> t
		| If_Else (_, _, _, t) -> t
		| If (_, _, t) -> t
		| Write (_, t) -> t
		| WriteLn (_, t) -> t
		| Read _ -> TUnit
		| ReadLn _ -> TUnit
		| Seq (_, _, t) -> t
		| CallProc (_, _, t) -> t

let get_type_decl d =
	match d with
		| Vars (_, t) -> t
		| Consts (_, t) -> t
		| Operations (_, t) -> t

let get_type_oper o =
	match o with
		| Function (_, _, _, _, t) -> t
		| Procedure (_, _, _, _, t) -> t

let get_type_program p =
	match p with
		| Program (_, _, _, t) -> t

