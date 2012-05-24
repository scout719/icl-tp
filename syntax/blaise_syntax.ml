open Blaise_iType;;

type expr = 
	| Number of int
	| String of string
	| Boolean of bool
	| Array of expr list * iType
	| Record of (string * expr) list * iType
	| New of expr * iType
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
	| Vars of (iType * (string list)) list
	| Operations of oper list * iType

and oper = 
	| Function of string * ((string * iType) list) * (decl_block list) * statement * iType
	| Procedure of string * ((string * iType) list) * (decl_block list) * statement * iType
	| Class of string * (decl_block list) * statement * iType;;

type program = 
	| Program of string * (decl_block list) * statement * iType;;

let get_type e =
	match e with
		| Number _ -> TNumber
		| String _ -> TString
		| Boolean _ -> TBoolean
		| Array(_,t) -> t 
		| Record(_,t) -> t
		| New (_, t) -> t
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
		| CallFun(_,_,t) -> t;;

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
		| CallProc (_, _, t) -> t;;

let get_type_decl d =
	match d with
		| Vars _ -> TUnit
		| Consts (_, t) -> t
		| Operations (_, t) -> t;;

let get_type_oper o =
	match o with
		| Function (_, _, _, _, t) -> t
		| Procedure (_, _, _, _, t) -> t
		| Class (_, _, _, t) -> t;;

let get_type_program p =
	match p with
		| Program (_, _, _, t) -> t;;

let rec unparse_bin_op l r t oper =
	let l_unparsed = unparse_expr l in
	let r_unparsed = unparse_expr r in
		oper ^ " ( " ^ l_unparsed ^ ", " ^ r_unparsed ^ ", " ^ (string_of_iType t) ^ " ) "

and unparse_un_op e t oper =
	let e_unparsed = unparse_expr e in
		oper ^ " ( " ^ e_unparsed ^ ", " ^ (string_of_iType t) ^ " ) "

and unparse_expr e =
	match e with
		| Number n -> "Number ( " ^ (string_of_int n) ^ " ) "
		
		| String s -> "String ( " ^ s ^ " ) "
		
		| Boolean b -> "Boolean ( " ^ (string_of_bool b) ^ " ) "
		
		| Record (list, t) ->
					let all_fields = List.fold_left (fun prev_unparse (s, e) ->
								let e_unparsed = unparse_expr e in
									prev_unparse @ ["( " ^ s ^ ", " ^ e_unparsed ^ " )"]
																					) [] list in
					let fields_unparsed = String.concat "; " all_fields in
						"Record ( [" ^ fields_unparsed ^ "], " ^ (string_of_iType t) ^ " ) "
		
		| Array (list, t) ->
					let all_elems = List.fold_left (fun prev_unparse e ->
								let e_unparsed = unparse_expr e in
									prev_unparse @ [e_unparsed]
																					) [] list in
					let elems_unparsed = String.concat "; " all_elems in
						"Array ( [" ^ elems_unparsed ^ "], " ^ (string_of_iType t) ^ " ) "
		
		| New (e, t) ->
					"New ( " ^ (unparse_expr e) ^ ", " ^ (string_of_iType t) ^ " ) "
		
		| Add (l, r, t) ->
					unparse_bin_op l r t "Add"
		
		| Sub (l, r, t) ->
					unparse_bin_op l r t "Sub"
		
		| Mult (l, r, t) ->
					unparse_bin_op l r t "Mult"
		
		| Div (l, r, t) ->
					unparse_bin_op l r t "Div"
		
		| Mod (l, r, t) ->
					unparse_bin_op l r t "Mod"
		
		| Compl (e, t) ->
					unparse_un_op e t "Compl"
		
		| And (l, r, t) ->
					unparse_bin_op l r t "And"
		
		| Or (l, r, t) ->
					unparse_bin_op l r t "Or"
		
		| Not (e, t) ->
					unparse_un_op e t "Not"
		
		| Gt (l, r, t) ->
					unparse_bin_op l r t "Gt"
		
		| Lt (l, r, t) ->
					unparse_bin_op l r t "Lt"
		
		| Eq (l, r, t) ->
					unparse_bin_op l r t "Eq"
		
		| Neq (l, r, t) ->
					unparse_bin_op l r t "Neq"
		
		| Gteq (l, r, t) ->
					unparse_bin_op l r t "Gteq"
		
		| Lteq (l, r, t) ->
					unparse_bin_op l r t "Lteq"
		
		| Id (s, t) ->
					"Id ( " ^ s ^ ", " ^ (string_of_iType t) ^ " ) "
		
		| GetArray (array, index, t) ->
					let array_unparsed = unparse_expr array in
					let index_unparsed = unparse_expr index in
						"GetArray ( " ^ array_unparsed ^ ", " ^ index_unparsed ^ ", " ^ (string_of_iType t) ^ " ) "
		
		| GetRecord (record, field, t) ->
					let record_unparsed = unparse_expr record in
						"GetRecord ( " ^ record_unparsed ^ ", " ^ field ^ ", " ^ (string_of_iType t) ^ " ) "
						
		| CallFun (e, e_list, t) ->
					let all_expr = List.fold_left (fun prev_unparse e ->
								let e_unparsed = unparse_expr e in
									prev_unparse @ [e_unparsed]
																				) [] e_list in
					let closure_unparsed = unparse_expr e in
					let expr_unparsed = String.concat "; " all_expr in
						"CallFun ( " ^ closure_unparsed ^ ", [" ^ expr_unparsed ^ "], " ^ (string_of_iType t) ^ " ) "

let rec unparse_statement s =
	match s with
		| Assign (l, r, t) ->
					let l_unparsed = unparse_expr l in
					let r_unparsed = unparse_expr r in
						"Assign ( " ^ l_unparsed ^ ", " ^ r_unparsed ^ ", " ^ (string_of_iType t) ^ " ) "

		| Seq (l, r, t) ->
					let l_unparsed = unparse_statement l in
					let r_unparsed = unparse_statement r in
						"Seq ( " ^ l_unparsed ^ ", " ^ r_unparsed ^ ", " ^ (string_of_iType t) ^ " ) "

		| If (e, s, t) ->
					let e_unparsed = unparse_expr e in
					let s_unparsed = unparse_statement s in
						"If ( " ^ e_unparsed ^ ", " ^ s_unparsed ^ ", " ^ (string_of_iType t) ^ " ) "

		| If_Else (e, s1, s2, t) ->
					let e_unparsed = unparse_expr e in
					let s1_unparsed = unparse_statement s1 in
					let s2_unparsed = unparse_statement s2 in
						"If ( " ^ e_unparsed ^ ", " ^ s1_unparsed ^ ", " ^ s2_unparsed ^ ", " ^ (string_of_iType t) ^ " ) "

		| While (e, s, t) ->
					let e_unparsed = unparse_expr e in
					let s_unparsed = unparse_statement s in
						"While ( " ^ e_unparsed ^ ", " ^ s_unparsed ^ ", " ^ (string_of_iType t) ^ " ) "

		| Write (list, t) ->
					let all_expr = List.fold_left (fun prev_unparse e ->
								let e_unparsed = unparse_expr e in
									prev_unparse @ [e_unparsed]
																				) [] list in
					let expr_unparsed = String.concat "; " all_expr in
						"Write ( [" ^ expr_unparsed ^ "], " ^ (string_of_iType t) ^ " ) "

		| WriteLn (list, t) ->
					let all_expr = List.fold_left (fun prev_unparse e ->
								let e_unparsed = unparse_expr e in
									prev_unparse @ [e_unparsed]
																				) [] list in
					let expr_unparsed = String.concat "; " all_expr in
						"WriteLn ( [" ^ expr_unparsed ^ "], " ^ (string_of_iType t) ^ " ) "

		| Read (list, t_list, t) ->
					let all_types = List.fold_left (fun prev_unparse t ->
								prev_unparse @ [string_of_iType t]
																					) [] t_list in
					let vars_unparsed = String.concat "; " list in
					let types_unparsed = String.concat "; " all_types in
						"Read ( [" ^ vars_unparsed ^ "], [" ^ types_unparsed ^ "], " ^ (string_of_iType t) ^ " ) "

		| ReadLn (list, t_list, t) ->
					let all_types = List.fold_left (fun prev_unparse t ->
								prev_unparse @ [string_of_iType t]
																					) [] t_list in
					let vars_unparsed = String.concat "; " list in
					let types_unparsed = String.concat "; " all_types in
						"ReadLn ( [" ^ vars_unparsed ^ "], [" ^ types_unparsed ^ "], " ^ (string_of_iType t) ^ " ) "
		
		| CallProc (e, e_list, t) ->
					let all_expr = List.fold_left (fun prev_unparse e ->
								let e_unparsed = unparse_expr e in
									prev_unparse @ [e_unparsed]
																				) [] e_list in
					let closure_unparsed = unparse_expr e in
					let expr_unparsed = String.concat "; " all_expr in
						"CallProc ( " ^ closure_unparsed ^ ", [" ^ expr_unparsed ^ "], " ^ (string_of_iType t) ^ " ) "

let rec unparse_oper op =
	match op with
		| Function (name, args_list, decl_block, statement, t) ->
					let all_args = List.fold_left (fun prev_unparse (s, t1) -> 
								prev_unparse @ ["( " ^ s ^ ", " ^ (string_of_iType t) ^ " )"]
																				) [] args_list in
					let args_unparsed = String.concat "; " all_args in
					let decl_unparsed = unparse_all_decls decl_block in
					let statement_unparsed = unparse_statement statement in
						"Function ( " ^ name ^ ", [" ^ args_unparsed ^ "], [" ^ decl_unparsed ^ "], " ^ statement_unparsed ^ ", " ^ (string_of_iType t) ^ " ) "

		| Procedure (name, args_list, decl_block, statement, t) ->
					let all_args = List.fold_left (fun prev_unparse (s, t1) -> 
								prev_unparse @ ["( " ^ s ^ ", " ^ (string_of_iType t) ^ " )"]
																				) [] args_list in
					let args_unparsed = String.concat "; " all_args in
					let decl_unparsed = unparse_all_decls decl_block in
					let statement_unparsed = unparse_statement statement in
						 "Procedure ( " ^ name ^ ", [" ^ args_unparsed ^ "], [" ^ decl_unparsed ^ "], " ^ statement_unparsed ^ ", " ^ (string_of_iType t) ^ " ) "

		| Class (name, decl_block, statement, t) ->
					let decl_unparsed = unparse_all_decls decl_block in
					let statement_unparsed = unparse_statement statement in
					 "Class ( " ^ name ^ ", [" ^ decl_unparsed ^ "], " ^ statement_unparsed ^ ", " ^ (string_of_iType t) ^ " ) "

and unparse_decl d =
	match d with
		| Consts (list, t) -> 
					let all_consts = List.fold_left (fun prev_unparse (s, e) ->
								let e_unparsed = unparse_expr e in
									prev_unparse @ ["( "^ s ^ ", " ^ e_unparsed ^" )"]
																								) [] list in
					let consts_unparsed = String.concat "; " all_consts in
						"Consts ( " ^ consts_unparsed ^ ", " ^ (string_of_iType t) ^ " ) "

		| Vars (list) -> 
					let all_vars = List.fold_left (fun prev_unparse (t1, list1) ->
								let list_unparsed = String.concat "; " list1 in
								let temp_vars_unparsed = "( " ^ (string_of_iType t1) ^ ", [" ^ list_unparsed ^ "] ) " in
									prev_unparse @ [temp_vars_unparsed]
																				) [] list in
					let vars_unparsed = String.concat "; " all_vars in
						"Vars ( " ^ vars_unparsed ^ " ) "

		| Operations (list, t) -> 
					let all_opers = List.fold_left (fun prev_unparse op ->
								let oper_unparsed = unparse_oper op in
									prev_unparse @ [oper_unparsed]
																					) [] list in
					let opers_unparsed = String.concat "; " all_opers in
						"Operations ( " ^ opers_unparsed ^ ", " ^ (string_of_iType t) ^ " ) "

and unparse_all_decls decl_block = 
	match decl_block with
		| [consts; vars; opers] ->
					let consts_unparsed = unparse_decl consts in
          let vars_unparsed	= unparse_decl vars in
          let opers_unparsed = unparse_decl opers in
          	String.concat "; " (consts_unparsed :: vars_unparsed :: opers_unparsed :: [])

		| _ -> "Internal error"

let unparse_program (Program (name, decl_block, statement, t)) =
	"Program ( " ^ name ^ ", [" ^ (unparse_all_decls decl_block) ^ "], " ^ (unparse_statement statement) ^ ", " ^ (string_of_iType t) ^ " ) ";;
