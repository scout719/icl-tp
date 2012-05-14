open Blaise_syntax
open Blaise_iType

exception Invalid_type of string
exception Id_not_found of string
exception Id_already_declared of string

module TypeEnvMap = Map.Make (String)

type env = string * iType TypeEnvMap.t

let find s env =
	try
		TypeEnvMap.find s env
	with Not_found -> raise (Id_not_found s)
	
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
										raise (Id_already_declared x)
									else
										()) list;;

let to_result t =
	match t with
		| TRef r -> !r
		| _ -> t;;

let rec check_assign l r =
	match l with
		| TRef lr -> 
				( match !lr, r with
					| TArray (length1, t1), TArray (length2, t2) -> 
								if length1 = length2 then 
									check_assign t1 (to_result t2)
								else
									TNone

					| TRecord (list1), TRecord (list2) -> 
								List.fold_left2 ( fun prev_type (s1, t1) (s2, t2) ->
																			if prev_type = TUnit && s1 = s2 then
																				check_assign t1 (to_result t2)
																			else
																				TNone
																) TUnit list1 list2
																
					| TFun (list1, t1), TFun (list2, t2) ->
								let matching_args = List.fold_left2 (fun prev_match t1' t2' ->
																												t1' = t2' && prev_match
																										) true list1 list2 in
									if matching_args && t1 = t2 then
										TUnit
									else
										TNone
																
					| TProc list1, TProc list2 ->
								let matching_args = List.fold_left2 (fun prev_match t1' t2' ->
																												t1' = t2' && prev_match
																										) true list1 list2 in
									if matching_args then
										TUnit
									else
										TNone
					
					| _, TRef r2 -> 
								if !lr = !r2 then
									TUnit
								else
									TNone
										
					| tl, tr -> 
								if tl = (to_result tr) then
									TUnit
								else
									TNone
				)
		| _ -> TNone;;

let rec typechk_exp env e =
	let typechk_exp' = typechk_exp env in
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
												let t = to_result (get_type e') in
  												if 	prev_type <> TNone && 
															(t = prev_type || prev_type = TUndefined) then
  													(prev_list @ [e'], t)
  												else 
  													(prev_list @ [e'], TNone)
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
											
		| Add (e1, e2, _) -> 
					let e1', e2' = typechk_exp' e1, typechk_exp' e2 in
					let t1, t2 = to_result (get_type e1'), to_result (get_type e2') in
						if (bin_oper_int t1 t2) then 
							Add(e1', e2', TNumber)
						else if (bin_oper_str t1 t2) then 
							Add(e1', e2', TString)
						else 
							Add(e1', e2', TNone)

		| Sub (e1, e2, _) -> 
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = to_result (get_type e1'), to_result (get_type e2') in
						if (bin_oper_int t1 t2) then 
							Sub(e1', e2', TNumber)
						else
							Sub(e1', e2', TNone)

		| Compl (e, _) ->
					let e' = typechk_exp' e in
					let t = to_result (get_type e') in
						if (un_oper_int t) then 
							Compl(e', TNumber)
						else
							Compl(e', TNone)

		| Mult (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = to_result (get_type e1'), to_result (get_type e2') in
						if (bin_oper_int t1 t2) then 
							Mult(e1', e2', TNumber)
						else
							Mult(e1', e2', TNone)

		| Div (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = to_result (get_type e1'), to_result (get_type e2') in
						if (bin_oper_int t1 t2) then 
							Div(e1', e2', TNumber)
						else
							Div(e1', e2', TNone)

		| Mod (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = to_result (get_type e1'), to_result (get_type e2') in
						if (bin_oper_int t1 t2) then 
							Mod(e1', e2', TNumber)
						else
							Mod(e1', e2', TNone)

		| Eq (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = to_result (get_type e1'), to_result (get_type e2') in
						if (bin_oper_bool t1 t2) || (bin_oper_int t1 t2) then 
							Eq(e1', e2', TBoolean)
						else
							Eq(e1', e2', TNone)

		| Neq (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = to_result (get_type e1'), to_result (get_type e2') in
						if (bin_oper_bool t1 t2) || (bin_oper_int t1 t2) then 
							Neq(e1', e2', TBoolean)
						else
							Neq(e1', e2', TNone)

		| Gt (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = to_result (get_type e1'), to_result (get_type e2') in
						if (bin_oper_bool t1 t2) || (bin_oper_int t1 t2) then 
							Gt(e1', e2', TBoolean)
						else
							Gt(e1', e2', TNone)

		| Lt (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = to_result (get_type e1'), to_result (get_type e2') in
						if (bin_oper_bool t1 t2) || (bin_oper_int t1 t2) then 
							Lt(e1', e2', TBoolean)
						else
							Lt(e1', e2', TNone)

		| Gteq (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = to_result (get_type e1'), to_result (get_type e2') in
						if (bin_oper_bool t1 t2) || (bin_oper_int t1 t2) then 
							Gteq(e1', e2', TBoolean)
						else
							Gteq(e1', e2', TNone)

		| Lteq (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = to_result (get_type e1'), to_result (get_type e2') in
						if (bin_oper_bool t1 t2) || (bin_oper_int t1 t2) then 
							Lteq(e1', e2', TBoolean)
						else
							Lteq(e1', e2', TNone)

		| And (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = to_result (get_type e1'), to_result (get_type e2') in
						if (bin_oper_bool t1 t2)then 
							And(e1', e2', TBoolean)
						else
							And(e1', e2', TNone)

		| Or (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = to_result (get_type e1'), to_result (get_type e2') in
						if (bin_oper_bool t1 t2)then 
							Or(e1', e2', TBoolean)
						else
							Or(e1', e2', TNone)

		| Not (e, _) ->
					let e' = typechk_exp' e in
					let t = to_result (get_type e') in
						if (un_oper_bool t)then 
							Not(e', TBoolean)
						else
							Not(e', TNone)

		| Id (s, _) -> 
					Id(s, find s env)

		| GetArray(e1, e2, _) ->
					let e1',e2' = typechk_exp' e1,typechk_exp' e2 in
					let t1, t2 = to_result (get_type e1'), to_result (get_type e2') in
					let array_type = get_type_of_array t1 in
						if (un_oper_array t1) && (un_oper_int t2) then
							GetArray(e1', e2', array_type)
						else 
							GetArray(e1', e2', TNone)

		| GetRecord(e, s, _) -> 
					let e' = typechk_exp' e in
					let t = to_result (get_type e') in
					let record_type = get_type_of_record s t in
						if un_oper_record s t then
							GetRecord(e', s, record_type)
						else
							GetRecord(e', s, TNone)

		| CallFun(e, args_list, _) ->
					let e' = typechk_exp' e in
					let t = to_result (get_type e') in
					let args_list' = 
								List.map( fun e -> typechk_exp' e) args_list in
						(match t with
							| TFun (params_types, t) -> 
										let matching_types = List.fold_left2 (fun prev_match e' t2 ->
																													let t1 = to_result (get_type e') in
																														t2 = t1 && prev_match
																													) true args_list' params_types in
											if matching_types then
												CallFun(e', args_list', t)
											else
												CallFun(e', args_list', TNone)
							| _ -> CallFun(e', args_list', TNone)
						);;

let rec typechk_stat env s =
	let typechk_exp' = typechk_exp env in
	let typechk_stat' = typechk_stat env in
		match s with
			| Assign (l, r, _) ->
						let l', r' = typechk_exp' l, typechk_exp' r in
						let t1, t2 = get_type l', to_result (get_type r') in
						let assign_type = check_assign t1 t2 in
							Assign(l', r', assign_type)
								
			| Seq (l, r, _) ->
						let l', r' = typechk_stat' l, typechk_stat' r in
						let t1, t2 = get_type_stat l', get_type_stat r' in
						let seq_type = if t1 = TUnit && t2 = TUnit then TUnit else TNone in
								Seq(l', r', seq_type)
			
			| If (e, s, _) -> 
						let e', s' = typechk_exp' e, typechk_stat' s in
						let t1, t2 = to_result (get_type e'), get_type_stat s' in
						let if_type = if t1 = TBoolean then t2 else TNone in
							If(e', s', if_type)
			
			| If_Else (e, s1, s2, _) ->
						let e', s1',s2' = typechk_exp' e, 
															typechk_stat' s1, 
															typechk_stat' s2 in
						let t1, t2,t3 = to_result (get_type e'), 
														get_type_stat s1', 
														get_type_stat s2' in
						let if_type = if 	t1 = TBoolean && 
															t2 = TUnit && 
															t3 = TUnit then TUnit else TNone in
							If_Else(e', s1', s2', if_type)
			
			| While (e, s, _) ->
						let e', s' = typechk_exp' e, typechk_stat' s in
						let t1, t2 = to_result (get_type e'), get_type_stat s' in
						let while_type = if t1 = TBoolean then t2 else TNone in
							While(e', s', while_type)
			
			| Write (list, _) -> 
						let list', writable = 
							List.fold_left ( fun (prev_list, prev_writable) e -> 
																		let e' = typechk_exp' e in
																		let t = to_result (get_type e') in
																			(prev_list @ [e'], is_writable t &&	prev_writable)
															) ([], true) list in
						let write_type = if writable then TUnit else TNone in
							Write (list', write_type)
			
			| WriteLn (list, t) ->
						let aux = typechk_stat' (Write(list, t)) in
						( match aux with
								| Write(list2, t2) -> WriteLn(list2, t2)
								| _ -> raise (Invalid_type "") (* dummy *)
						)
			
			| Read (list, _) ->
						let readable = 
							List.fold_left (fun prev_readable s ->
																	let t = find s env in
																		is_readable t && prev_readable
														) true list in
						let read_type = if readable then TUnit else TNone in
							Read (list, read_type)
			
			| ReadLn (list, t) ->
						let aux = typechk_stat' (Read(list, t)) in
						( match aux with
							| Read (list2, t2) -> ReadLn(list2, t2)
							| _ -> raise (Invalid_type "") (* dummy *)
						)
			
  		| CallProc(e, args_list, _) -> 
  					let e' = typechk_exp' e in
  					let t = to_result (get_type e') in
  					let args_list' = 
  								List.map( fun e -> typechk_exp' e) args_list in
  						(match t with
  							| TProc params_types -> 
  										let matching_types = List.fold_left2 (fun prev_match e' t2 ->
        																												let t1 = to_result (get_type e') in
        																													t2 = t1 && prev_match
  																													) true args_list' params_types in
  											if matching_types then
  												CallProc(e', args_list', TUnit)
  											else
  												CallProc(e', args_list', TNone)
  							| _ -> CallProc(e', args_list', TNone)
  						)
						
and typechk_all_decls env args_list consts vars opers =
		let all_args, new_env = List.fold_left (
																	fun (prev_args, prev_env) (s, t) -> 
																			(prev_args @ [s], assoc s t prev_env)
																					) ([], env) args_list in
			check_duplicates all_args;
			let all_consts ,consts', env_consts = typechk_decl new_env consts in
			let all_vars, vars', env_vars = typechk_decl env_consts vars in
			let all_opers, opers', env_opers = typechk_decl env_vars opers in
				check_duplicates (all_consts @ all_vars @ all_opers);
				let consts_type = get_type_decl consts' in
				let vars_type = get_type_decl vars' in
				let opers_type = get_type_decl opers' in
					if consts_type = TUnit &&
							vars_type = TUnit &&
							opers_type = TUnit then
						([consts'; vars'; opers'], env_opers, TUnit)
					else
						([consts'; vars'; opers'], env_opers, TNone)

and typechk_oper env o =
	match o with
		| Function (name, args_list, [consts; vars; opers],  s, t) -> 
					let decl_block, temp_env, decl_type = typechk_all_decls env args_list consts vars opers in
					let args_type_list = List.map (fun (_, t) -> t) args_list in
					let return_type_ref = ref t in
					let recursive_env = assoc name (TFun ( args_type_list, t)) temp_env in
					let new_env = assoc "result" (TRef(return_type_ref)) recursive_env in
					let s' = typechk_stat new_env s in
					let fun_type = if (get_type_stat s') = TUnit then t else TNone in
					let final_env = assoc name (TFun ( args_type_list, fun_type)) env in
						(name, Function (name, args_list, decl_block, s', fun_type), final_env)
		
		| Procedure (name, args_list, [consts; vars; opers], s, _) -> 
					let decl_block, temp_env, decl_type = typechk_all_decls env args_list consts vars opers in
					let args_type_list = List.map (fun (_, t) -> t) args_list in
					let recursive_env = assoc name TUnit temp_env in
					let s' = typechk_stat recursive_env s in
					let s_type = get_type_stat s' in
					let final_env = assoc name (TProc ( args_type_list)) env in
						(name, Procedure (name, args_list, decl_block, s', s_type), final_env)
		
		| _ -> raise (Invalid_type "") (* dummy *)

and typechk_decl env d =
		match d with	
			| Vars (list, _) -> 
      			let all_vars, vars_type, vars_env =
							List.fold_left (fun (prev_vars, prev_checked ,prev_env) (t, l) -> 
      														let temp_env = 
      															List.fold_left (fun prev s -> 
      																										assoc s (get_reference_to t) prev
      																							) prev_env l in
      															(prev_vars @ l, TUnit, temp_env)
      												) ([], TUnit, env) list in
							(all_vars, Vars (list, vars_type), vars_env)
			
    	| Consts (list, _) -> 
      			let all_consts, consts', consts_type, consts_env =
							List.fold_left (fun (prev_consts, prev_checked, prev_type, prev_env) (s, e) -> 
																	let e' = typechk_exp prev_env e in
																	let t = get_type e' in
																		if t <> TNone && prev_type <> TNone then
																			(prev_consts @ [s], prev_checked @ [(s, e')], TUnit, assoc s t prev_env)
																		else
																			(prev_consts @ [s], prev_checked @ [(s, e')], TNone, assoc s t prev_env)
															) ([], [], TUnit, env) list in
							(all_consts, Consts(consts', consts_type), consts_env)
			
			| Operations (list, _) ->
      			let all_opers, opers', opers_type, opers_env = 
							List.fold_left (fun (prev_opers, prev_checked, prev_type, prev_env) o ->
      															let (name, oper', new_env) = typechk_oper prev_env o in
																		let t = get_type_oper oper' in
  																		if t <> TNone && prev_type <> TNone then
  																			(prev_opers @ [name], prev_checked @ [oper'], TUnit, new_env)
  																		else
  																			(prev_opers @ [name], prev_checked @ [oper'], TNone, new_env)
      												) ([], [], TUnit, env) list in
							(all_opers, Operations(opers', opers_type), opers_env);;

let typechk_program p =
	match p with
		| Program(name, [consts; vars; opers], s, _) -> 
				let decl_block , env, decl_type = typechk_all_decls TypeEnvMap.empty [] consts vars opers in
				let s' = typechk_stat env s in
				let t = get_type_stat s' in
					if decl_type <> TNone && t <> TNone then
						Program(name, decl_block, s', TUnit)
					else
						Program(name, decl_block, s', TNone)
								
		| _ -> raise (Invalid_type "") (* dummy *);;
