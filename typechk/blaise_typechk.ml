open Blaise_syntax
open Blaise_iType

exception Type_check_error of string;;

module TypeEnvMap = Map.Make (String);;

type env = string * iType TypeEnvMap.t;;

let rec find s env =
	try
		TypeEnvMap.find s env
	with Not_found -> raise (Type_check_error ("Id not found: " ^ s));;
	
let assoc k v env =
	TypeEnvMap.add k v env

(* Funcao que retorna () se nao houver variaveis duplicadas na lista *)
(* caso contrario lanca Variable_already_declared x *)
let check_duplicates list =
	List.iter (fun x -> 
								let all = List.find_all (fun s -> 
																							s = x
																				) list in
									if List.length all <> 1 then
										raise (Type_check_error ("Id already declared: " ^ x))
									else
										()) list;;

let rec check_assign env l r =
	(* print_string ("left: "^(string_of_iType l)^"\nright: "^(string_of_iType r)^"\n\n"); *)
	match l with
		| TRef lr -> 
				( match !lr, r with
					| TArray (length1, t1), TArray (length2, t2) -> 
								if length1 = length2 then 
									check_assign env t1 t2
								else
									TNone "Arrays with diferent lengths"

					| TRecord (list1), TRecord (list2) -> 
								List.fold_left2 ( fun prev_type (s1, t1) (s2, t2) ->
																			if prev_type = TUnit && s1 = s2 then
																				check_assign env t1 t2
																			else
																				TNone "Records with diferent fields"
																) TUnit list1 list2

					| TFun (list1, t1), TFun (list2, t2) ->
								if equals [] [] !lr r then
									TUnit
								else
									TNone "Functions with diferent parameters"

					| TProc list1, TProc list2 ->
								if equals [] [] !lr r then
									TUnit
								else
									TNone "Procedures with diferent parameters"

					| tl, tr -> 
								if equals [] [] tl (unref_iType tr) then
									TUnit
								else
									TNone "Types not matching"
				)
		| _ -> TNone "TRef expected on left of assign";;

let rec check_matching_types env t1 t2 =
	equals [] [] t1 t2
				
let get_oper_info env oper = 
	match oper with
		| Function(name, list, _, _, t) -> 
					let args_types_list = List.map (fun (_, t) -> t) list in
						(name, TFun(args_types_list, t))

		| Procedure(name, list, _, _, t) -> 
					let args_types_list = List.map (fun (_, t) -> t) list in
						(name, TProc(args_types_list))

		| _ -> ("", TNone "dummy");; (* dummy *)

let get_methods env opers =
	match opers with
		| Operations(opers, _) ->
					List.map (fun oper -> get_oper_info env oper) opers
		
		| _ -> [];; (* dummy *)

let get_self_record self_type =
	match self_type with
		| TRecord list ->
					let new_list = List.map (fun (s, t) -> (s, Id(s, t))) list in
          	Record(new_list, self_type)
		
		| _ -> Record([], TNone "dummy");; (* dummy *)

let rec is_class t =
	match t with
		| TClass _ -> true
		| _ -> false;;

let get_object_type t =
	match t with
		| TClass (name, list) -> TObject (name, list)
		| _ -> TNone "dummy" (* dummy *)

let rec typechk_exp env e =
	let typechk_exp' = typechk_exp env in
	let equals' = equals [] [] in
	match e with
		| Number n -> Number n
		
		| String s -> String s
		
		| Boolean b -> Boolean b
		
		| Array(list, _) -> 
					let size = List.length list in
					let list', array_type =
								List.fold_left (
										fun (prev_list, prev_type) e -> 
  											let e' = typechk_exp' e in
												let t = unref_iType (get_type e') in
  												if 	not_none prev_type && 
															(equals' t prev_type) || (equals' prev_type TUndefined) then
  													(prev_list @ [e'], t)
  												else 
  													(prev_list @ [e'], TNone "elements with diferent types")
																) ([], TUndefined) list in
						Array(list', TArray(size, array_type))
		
		| Record(list, _) -> 
					let list', record_type_list = 
						List.fold_left (
										fun (prev_list, prev_record_type) (s, e) -> 
												let e' = typechk_exp' e in
												let t = get_type e' in
													(prev_list @ [(s, e')], prev_record_type @ [(s, t)])
														) ([], []) list in
						Record(list', TRecord(record_type_list))
		
		| New (e, _) -> 
					let e' = typechk_exp' e in
					let t = unref_iType (get_type e') in
						if is_class t then (
							let object_type = get_object_type t in
								New(e', object_type)	
					) else
							New(e', TNone "Class expected in New")
		
		| Add (e1, e2, _) -> 
					let e1', e2' = typechk_exp' e1, typechk_exp' e2 in
					let t1, t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
						if (bin_oper_int t1 t2) then 
							Add(e1', e2', TNumber)
						else if (bin_oper_str t1 t2) then 
							Add(e1', e2', TString)
						else 
							Add(e1', e2', TNone "Invalid types in Add")

		| Sub (e1, e2, _) -> 
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
						if (bin_oper_int t1 t2) then 
							Sub(e1', e2', TNumber)
						else
							Sub(e1', e2', TNone "Invalid types in Sub")

		| Compl (e, _) ->
					let e' = typechk_exp' e in
					let t = unref_iType (get_type e') in
						if (un_oper_int t) then 
							Compl(e', TNumber)
						else
							Compl(e', TNone "Invalid type in Compl")

		| Mult (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
						if (bin_oper_int t1 t2) then 
							Mult(e1', e2', TNumber)
						else
							Mult(e1', e2', TNone "Invalid types in Mult")

		| Div (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
						if (bin_oper_int t1 t2) then 
							Div(e1', e2', TNumber)
						else
							Div(e1', e2', TNone "Invalid types in Div")

		| Mod (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
						if (bin_oper_int t1 t2) then 
							Mod(e1', e2', TNumber)
						else
							Mod(e1', e2', TNone "Invalid types in Mod")

		| Eq (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
						if (bin_oper_str t1 t2) || (bin_oper_int t1 t2) then 
							Eq(e1', e2', TBoolean)
						else
							Eq(e1', e2', TNone "Invalid types in Eq")

		| Neq (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
						if (bin_oper_str t1 t2) || (bin_oper_int t1 t2) then 
							Neq(e1', e2', TBoolean)
						else
							Neq(e1', e2', TNone "Invalid types in Neq")

		| Gt (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
						if bin_oper_int t1 t2 then 
							Gt(e1', e2', TBoolean)
						else
							Gt(e1', e2', TNone "Invalid types in Gt")

		| Lt (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
						if bin_oper_int t1 t2 then 
							Lt(e1', e2', TBoolean)
						else
							Lt(e1', e2', TNone "Invalid types in Lt")

		| Gteq (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
						if bin_oper_int t1 t2 then 
							Gteq(e1', e2', TBoolean)
						else
							Gteq(e1', e2', TNone "Invalid types in Gteq")

		| Lteq (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
						if bin_oper_int t1 t2 then 
							Lteq(e1', e2', TBoolean)
						else
							Lteq(e1', e2', TNone "Invalid types in Lteq")

		| And (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
						if (bin_oper_bool t1 t2)then 
							And(e1', e2', TBoolean)
						else
							And(e1', e2', TNone "Invalid types in And")

		| Or (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
						if (bin_oper_bool t1 t2)then 
							Or(e1', e2', TBoolean)
						else
							Or(e1', e2', TNone "Invalid types in Or")

		| Not (e, _) ->
					let e' = typechk_exp' e in
					let t = unref_iType (get_type e') in
						if (un_oper_bool t)then 
							Not(e', TBoolean)
						else
							Not(e', TNone "Invalid type in Not")

		| Id (s, _) -> 
					let t = find s env in
						Id(s, t)

		| GetArray(e1, e2, _) ->
					let e1',e2' = typechk_exp' e1,typechk_exp' e2 in
					let t1, t2 = unref_iType (get_type e1'), unref_iType (get_type e2') in
					let array_type = get_type_of_array t1 in
						if (un_oper_array t1) && (un_oper_int t2) then
							GetArray(e1', e2', array_type)
						else
							GetArray(e1', e2', TNone "Invalid types in GetArray")

		| GetRecord(e, s, _) -> 
					let e' = typechk_exp' e in
					let t = unref_iType (get_type e') in
					let record_type = get_type_of_record env s t in
						if un_oper_record s t then
							GetRecord(e', s, record_type)
						else
							GetRecord(e', s, TNone "Invalid types in GetRecord")

		| CallFun(e, args_list, _) ->
					let e' = typechk_exp' e in
					let t = unref_iType (get_type e') in
					let args_list' = 
								List.map( fun e -> typechk_exp' e) args_list in
						(match t with
							| TFun (params_types, t) -> 
										let matching_types = List.fold_left2 (fun prev_match e' t2 ->
																													let t1 = unref_iType (get_type e') in
																														(check_matching_types env t2 t1) && prev_match
																													) true args_list' params_types in
											if matching_types then
												CallFun(e', args_list', t)
											else
												CallFun(e', args_list', TNone "Types of arguments don't match with function's parameters types")
							| _ -> CallFun(e', args_list', TNone "Invalid closure in CallFun")
						);;

let rec typechk_stat env s =
	let typechk_exp' = typechk_exp env in
	let typechk_stat' = typechk_stat env in
	let equals' = equals [] [] in
		match s with
			| Assign (l, r, _) ->
						let l', r' = typechk_exp' l, typechk_exp' r in
						let t1, t2 = get_type l', unref_iType (get_type r') in
						let assign_type = check_assign env t1 t2 in
							Assign(l', r', assign_type)
								
			| Seq (l, r, _) ->
						let l', r' = typechk_stat' l, typechk_stat' r in
						let t1, t2 = get_type_stat l', get_type_stat r' in
						let seq_type = if (equals' t1 TUnit) && (equals' t2 TUnit) then TUnit else TNone "Invalid types in Seq" in
								Seq(l', r', seq_type)
			
			| If (e, s, _) -> 
						let e', s' = typechk_exp' e, typechk_stat' s in
						let t1, t2 = unref_iType (get_type e'), get_type_stat s' in
						let if_type = if equals' t1 TBoolean then t2 else TNone "Invalid types in if" in
							If(e', s', if_type)
			
			| If_Else (e, s1, s2, _) ->
						let e', s1',s2' = typechk_exp' e, 
															typechk_stat' s1, 
															typechk_stat' s2 in
						let t1, t2,t3 = unref_iType (get_type e'), 
														get_type_stat s1', 
														get_type_stat s2' in
						let if_type = if 	(equals' t1 TBoolean) && 
															(equals' t2 TUnit) && 
															(equals' t3 TUnit) then TUnit else TNone "Invalid types in If_Else" in
							If_Else(e', s1', s2', if_type)
			
			| While (e, s, _) ->
						let e', s' = typechk_exp' e, typechk_stat' s in
						let t1, t2 = unref_iType (get_type e'), get_type_stat s' in
						let while_type = if (equals' t1 TBoolean) then t2 else TNone "Invalid types in While" in
							While(e', s', while_type)
			
			| Write (list, _) -> 
						let list', writable = 
							List.fold_left ( fun (prev_list, prev_writable) e -> 
																		let e' = typechk_exp' e in
																		let t = unref_iType (get_type e') in
																			(prev_list @ [e'], is_writable t &&	prev_writable)
															) ([], true) list in
						let write_type = if writable then TUnit else TNone "Invalid types in Write" in
							Write (list', write_type)
			
			| WriteLn (list, t) ->
						let aux = typechk_stat' (Write(list, t)) in
						( match aux with
								| Write(list2, t2) -> WriteLn(list2, t2)
								| _ -> raise (Type_check_error "Internal error: WriteLn()") (* dummy *)
						)
			
			| Read (list, _, _) ->
						let readable, read_types_list = 
							List.fold_left (fun (prev_readable, prev_list) s ->
																	let t = find s env in
																		(is_readable t && prev_readable, prev_list @ [t])
														) (true, []) list in
						let read_type = if readable then TUnit else TNone "Invalid types in Read" in
							Read (list, read_types_list, read_type)
			
			| ReadLn (list, tl, t) ->
						let aux = typechk_stat' (Read(list, tl, t)) in
						( match aux with
							| Read (list2, tl2, t2) -> ReadLn(list2, tl2, t2)
							| _ -> raise (Type_check_error "Internal error: ReadLn()") (* dummy *)
						)
			
  		| CallProc(e, args_list, _) -> 
  					let e' = typechk_exp' e in
  					let t = unref_iType (get_type e') in
  					let args_list' = List.map( fun e -> typechk_exp' e) args_list in
  						(match t with
  							| TProc params_types -> 
  										let matching_types = List.fold_left2 (fun prev_match e' t2 ->
        																										let t1 = unref_iType (get_type e') in
																															(check_matching_types env t2 t1) && prev_match
  																													) true args_list' params_types in
  											if matching_types then
  												CallProc(e', args_list', TUnit)
  											else
  												CallProc(e', args_list', TNone "Types of arguments don't match with procedure's parameters types")
  							| _ -> CallProc(e', args_list', TNone "Invalid closure in CallProc")
  						)
						
and typechk_all_decls env types consts vars opers =
			let all_consts ,consts', env_consts = typechk_decl env consts in
			let all_vars, vars', env_vars = typechk_decl env_consts vars in
			let all_opers, opers', env_opers = typechk_decl env_vars opers in
				check_duplicates (all_consts @ all_vars @ all_opers);
				(* let types_type = get_type_decl types' in *)
				let consts_type = get_type_decl consts' in
				let vars_type = get_type_decl vars' in
				let opers_type = get_type_decl opers' in
					if (*(equals [] [] types_type TUnit) &&*)
							(equals [] [] consts_type TUnit) &&
							(equals [] [] vars_type TUnit) &&
							(equals [] [] opers_type TUnit) then
						([types; consts'; vars'; opers'], env_opers, TUnit)
					else
						([types; consts'; vars'; opers'], env_opers, TNone "Decls not well typed")

and typechk_oper env o =
	match o with
		| Function (name, args_list, [types; consts; vars; opers],  s, t) -> 
      		let all_args, new_env = List.fold_left (
      																	fun (prev_args, prev_env) (s, t) -> 
      																			(prev_args @ [s], assoc s t prev_env)
      																					) ([], env) args_list in
      			check_duplicates all_args;
					let args_type_list = List.map (fun (_, t) -> t) args_list in
					let recursive_env = assoc name (TFun ( args_type_list, t)) new_env in
					let decl_block, temp_env, decl_type = typechk_all_decls recursive_env types consts vars opers in
					let new_env = assoc "result" (get_reference_to env t) temp_env in
					let s' = typechk_stat new_env s in
					let fun_type = if not_none (get_type_stat s') && not_none decl_type then t else TNone ("Invalid function: "^name) in
					let final_env = assoc name (TFun ( args_type_list, fun_type)) env in
						(name, Function (name, args_list, decl_block, s', fun_type), final_env)
		
		| Procedure (name, args_list, [types; consts; vars; opers], s, _) -> 
      		let all_args, new_env = List.fold_left (
      																	fun (prev_args, prev_env) (s, t) -> 
      																			(prev_args @ [s], assoc s t prev_env)
      																					) ([], env) args_list in
      			check_duplicates all_args;
					let args_type_list = List.map (fun (_, t) -> t) args_list in
					let recursive_env = assoc name (TProc (args_type_list)) new_env in
					let decl_block, temp_env, decl_type = typechk_all_decls recursive_env types consts vars opers in
					let s' = typechk_stat temp_env s in
					let s_type = get_type_stat s' in
					let final_env = assoc name (TProc ( args_type_list)) env in
					let final_type = if not_none decl_type && not_none s_type then TUnit else TNone ("Invalid procedure: "^name) in
						(name, Procedure (name, args_list, decl_block, s', final_type), final_env)
		
		| Class (name, [types; consts; vars; opers], statement, _) -> 
					let method_list = get_methods env opers in
					let class_type = TClass("", method_list) in
        	let self_type = TObject("", method_list) in
					let new_env1 = assoc name class_type env in
					let new_env = assoc "self" self_type new_env1 in
					let decl_block, temp_env, decl_type = typechk_all_decls new_env types consts vars opers in
					let statement' = typechk_stat temp_env statement in
					let stat_type = get_type_stat statement' in
					let final_type = if not_none stat_type && not_none decl_type then class_type else TNone ("Invalid Class: "^name) in
					let final_env = assoc name final_type env in
						(name, Class(name, decl_block, statement', final_type), final_env)
		
		| _ -> raise (Type_check_error "Internal error: operation expected") (* dummy *)

and typechk_decl env d =
		match d with	
			| Types list -> 
      			let all_types, types_env =
							List.fold_left (fun (prev_types, prev_env) (s, t) -> 
																			(prev_types @ [s], assoc s t prev_env)
															) ([], env) list in
							(all_types, Types list, types_env)
			| Vars list -> 
      			let all_vars, vars_env =
							List.fold_left (fun (prev_vars, prev_env) (t, l) -> 
      														let temp_env = 
      															List.fold_left (fun prev s -> 
      																										assoc s (get_reference_to env t) prev
      																							) prev_env l in
      															(prev_vars @ l, temp_env)
      												) ([], env) list in
							(all_vars, Vars list, vars_env)
			
    	| Consts (list, _) -> 
      			let all_consts, consts', consts_type, consts_env =
							List.fold_left (fun (prev_consts, prev_checked, prev_type, prev_env) (s, e) -> 
																	let e' = typechk_exp prev_env e in
																	let t = get_type e' in
																		if not_none t && not_none prev_type then
																			(prev_consts @ [s], prev_checked @ [(s, e')], TUnit, assoc s t prev_env)
																		else
																			(prev_consts @ [s], prev_checked @ [(s, e')], TNone "Const with errors", assoc s t prev_env)
															) ([], [], TUnit, env) list in
							(all_consts, Consts(consts', consts_type), consts_env)
			
			| Operations (list, _) ->
      			let all_opers, opers', opers_type, opers_env = 
							List.fold_left (fun (prev_opers, prev_checked, prev_type, prev_env) o ->
      															let (name, oper', new_env) = typechk_oper prev_env o in
																		let t = get_type_oper oper' in
  																		if not_none t && not_none prev_type then
  																			(prev_opers @ [name], prev_checked @ [oper'], TUnit, new_env)
  																		else
  																			(prev_opers @ [name], prev_checked @ [oper'], TNone "Operations with errors", new_env)
      												) ([], [], TUnit, env) list in
							(all_opers, Operations(opers', opers_type), opers_env);;

let typechk_program p =
	match p with
		| Program(name, [types; consts; vars; opers], s, _) -> 
				let decl_block , env, decl_type = typechk_all_decls TypeEnvMap.empty types consts vars opers in
				let s' = typechk_stat env s in
				let t = get_type_stat s' in
					if not_none decl_type && not_none t then
						Program(name, decl_block, s', TUnit)
					else
						Program(name, decl_block, s', TNone "Error in program")
								
		| _ -> raise (Type_check_error "Internal error: Program expected") (* dummy *);;
