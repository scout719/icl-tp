type iType =
	| TNumber
	| TString
	| TBoolean
	| TFun of (iType list) * iType
	| TProc of (iType list)
	| TArray of int * iType
	| TRecord of (string * iType) list
	| TRef of iType ref
	| TUnit
	| TNone
	| TUndefined

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
	| CallFun of expr * (expr list) * iType

type statement =
	| Assign of expr * expr * iType
	| While of expr * statement * iType
	| If_Else of expr * statement * statement *iType
	| If of expr * statement * iType
	| Write of expr list * iType
	| WriteLn of expr list * iType
	| Read of string list
	| ReadLn of string list
	| Seq of statement * statement * iType
	| CallProc of expr * (expr list) * iType

type decl_block =
	| Consts of (string * expr) list * iType
	| Vars of (iType * (string list)) list * iType
	| Operations of oper list * iType

and oper = 
	| Function of string * ((string * iType) list) * (decl_block list) * statement * iType
	| Procedure of string * ((string * iType) list) * (decl_block list) * statement * iType

type program = 
	| Program of string * (decl_block list) * statement * iType

let string_of_iType t =
	match t with
  	(*| TNumber -> "Number"
  	| TString -> "String"
  	| TBoolean -> "Boolean"
  	| TFun -> letof (iType list) * iType
  	| TProc of (iType list)
  	| TArray of int * iType
  	| TRecord of (string * iType) list
  	| TRef of iType ref *)
  	| TUnit -> "Unit"
  	| TNone -> "None"
  	| TUndefined -> "Undefined"
