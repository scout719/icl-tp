open Blaise_syntax

exception Invalid_type of string
exception Id_not_found of string
exception Id_already_declared of string

module TypeEnvMap = Map.Make (String)

type env = string*iType TypeEnvMap.t

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
										()) list

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

let rec bin_oper_int t1 t2 =
	match t1, t2 with
		| TRef r , _ -> bin_oper_int !r t2
		| _ , TRef r -> bin_oper_int t1 !r
		| TNumber, TNumber -> true
		| _ -> false
	
let rec bin_oper_str t1 t2 =
	match t1, t2 with
		| TRef r , _ -> bin_oper_str !r t2
		| _ , TRef r -> bin_oper_str t1 !r
		| TString, TString -> true
		| _ -> false

let rec bin_oper_bool t1 t2 =
	match t1, t2 with
		| TRef r , _ -> bin_oper_bool !r t2
		| _ , TRef r -> bin_oper_bool t1 !r
		| TBoolean, TBoolean -> true
		| _ -> false
	
let rec un_oper_int t =
	match t with
		| TRef r -> un_oper_int !r
		| TNumber -> true
		| _ -> false

let rec un_oper_str t =
	match t with
		| TRef r -> un_oper_str !r
		| TString -> true
		| _ -> false
	
let rec un_oper_bool t =
	match t with
		| TRef r -> un_oper_bool !r
		| TBoolean -> true
		| _ -> false

let rec un_oper_array t =
	match t with
		| TRef r -> un_oper_array !r
		| TArray _ -> true
		| _ -> false

let rec check_assign l r =
	match l with
		| TRef lr -> 
				( match !lr, r with
					| _, TRef rr -> check_assign l !rr
					| TArray (length1, t1), TArray (length2, t2) -> 
								if length1 = length2 && (check_assign (TRef (ref t1)) t2) = TUnit then
									TUnit
								else
									TNone
					| TRecord (list1), TRecord (list2) -> 
								List.fold_left2 ( fun prev_type (s1, t1) (s2, t2) ->
																			if prev_type = TUnit then
																				if (s1 = s2) && (check_assign (TRef (ref t1)) t2) = TUnit then
																					TUnit
																				else
																					TNone
																			else
																				TNone
																) TUnit list1 list2
					| tl, tr -> 
								if tl = tr then
									TUnit
								else
									TNone
				)
		| _ -> TNone

let rec typechk_exp env e =
	let typechk_exp' = typechk_exp env in
	match e with
		| Number n -> Number(n)
		
		| String s -> String(s)
		
		| Boolean b -> Boolean(b)
		
		| Array(list, _) -> 
					let size = List.length list in
					let checked_list, array_type =
								List.fold_left (
										fun (prev_list, prev_type) e -> 
  											let e_checked = typechk_exp' e in
												let e_type = get_type e_checked in
  												if prev_type = TNone then
  													(prev_list @ [e_checked], prev_type)
  												else (
  													if prev_type = e_type ||
  															(prev_type = TUndefined) then
  														(prev_list @ [e_checked], e_type)
  													else
  														(prev_list @ [e_checked], TNone)
  												)
																) ([], TNone) list in
						Array(checked_list, TArray(size, array_type))
		
		| Record(list, _) -> 
					let checked_list, record_type_list = 
						List.fold_left (
										fun (prev_checked, prev_list) (s, e) -> 
												let e_checked = typechk_exp' e in
												let e_type = get_type e_checked in
													(prev_checked @ [(s, e_checked)], prev_list @ [(s, e_type)])
													) ([], []) list in
						Record(checked_list, TRecord(record_type_list))
											
		| Add (e1, e2, _) -> 
					let e1', e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1, t2 = (get_type e1', get_type e2') in
						if (bin_oper_int t1 t2) then 
							Add(e1', e2', TNumber)
						else if (bin_oper_str t1 t2) then 
							Add(e1', e2', TString)
						else 
							raise (Invalid_type "")
													
			
		| Sub (e1, e2, _) -> 
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = (get_type e1', get_type e2') in
						if (bin_oper_int t1 t2) then 
							Sub(e1', e2', TNumber)
						else
							raise (Invalid_type "")
													
		| Compl (e, _) ->
					let e' = typechk_exp' e in
					let t = get_type e' in
						if (un_oper_int t) then 
							Compl(e', TNumber)
						else
							raise (Invalid_type "")
												
		| Mult (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = (get_type e1', get_type e2') in
						if (bin_oper_int t1 t2) then 
							Mult(e1', e2', TNumber)
						else
							raise (Invalid_type "")
													
		| Div (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = (get_type e1', get_type e2') in
						if (bin_oper_int t1 t2) then 
							Div(e1', e2', TNumber)
						else
							raise (Invalid_type "")
													
		| Mod (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = (get_type e1', get_type e2') in
						if (bin_oper_int t1 t2) then 
							Mod(e1', e2', TNumber)
						else
							raise (Invalid_type "")
													
		| Eq (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = (get_type e1', get_type e2') in
						if (bin_oper_bool t1 t2) || (bin_oper_int t1 t2) then 
							Eq(e1', e2', TBoolean)
						else
							raise (Invalid_type "")
													
		| Neq (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = (get_type e1', get_type e2') in
						if (bin_oper_bool t1 t2) || (bin_oper_int t1 t2) then 
							Neq(e1', e2', TBoolean)
						else
							raise (Invalid_type "")
													
		| Gt (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = (get_type e1', get_type e2') in
						if (bin_oper_bool t1 t2) || (bin_oper_int t1 t2) then 
							Gt(e1', e2', TBoolean)
						else
							raise (Invalid_type "")
													
		| Lt (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = (get_type e1', get_type e2') in
						if (bin_oper_bool t1 t2) || (bin_oper_int t1 t2) then 
							Gt(e1', e2', TBoolean)
						else
							raise (Invalid_type "")
													
		| Gteq (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = (get_type e1', get_type e2') in
						if (bin_oper_bool t1 t2) || (bin_oper_int t1 t2) then 
							Gteq(e1', e2', TBoolean)
						else
							raise (Invalid_type "")
													
		| Lteq (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = (get_type e1', get_type e2') in
						if (bin_oper_bool t1 t2) || (bin_oper_int t1 t2) then 
							Lteq(e1', e2', TBoolean)
						else
							raise (Invalid_type "")
													
		| And (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = (get_type e1', get_type e2') in
						if (bin_oper_bool t1 t2)then 
							And(e1', e2', TBoolean)
						else
							raise (Invalid_type "")
													
		| Or (e1, e2, _) ->
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1,t2 = (get_type e1', get_type e2') in
						if (bin_oper_bool t1 t2)then 
							Or(e1', e2', TBoolean)
						else
							raise (Invalid_type "")
													
		| Not (e, _) ->
					let e' = typechk_exp' e in
					let t = get_type e' in
						if (un_oper_bool t)then 
							Not(e', TBoolean)
						else
							raise (Invalid_type "")
											
		| Id (s, _) -> 
					Id(s,find s env)
		
		| GetArray(e1, e2, _) -> 
					let e1',e2' = (typechk_exp' e1,typechk_exp' e2) in
					let t1, t2 = (get_type e1', get_type e2') in
						if (un_oper_array t1) && (un_oper_int t2) then(
								GetArray(e1', e2', t2)
						)else 
							raise (Invalid_type "")
																	
		| _	-> Boolean(true) (* dummy *)
									
(*		| GetRecord (e1, s, t) -> *)
(*		| CallFun (e1,l,t) ->     *)

let rec typechk_stat env s =
	let typechk_exp' = typechk_exp env in
	let typechk_stat' = typechk_stat env in
		match s with
			| Assign (l, r, _) ->
						let l', r' = typechk_exp' l, typechk_exp' r in
						let t1, t2 = get_type l', get_type r' in
							if (check_assign t1 t2) = TUnit then
								Assign (l', r', TUnit)
							else
								Assign (l', r', TNone)
								
			| Seq (l, r, _) ->
						let l', r' = typechk_stat' l, typechk_stat' r in
						let t1, t2 = get_type_stat l', get_type_stat r' in
							if t1 = TUnit && t2 = TUnit then
								Seq(l', r', TUnit)
							else
								Seq(l', r', TNone)
			
			| If (e, s, _) -> 
						let e', s' = typechk_exp' e, typechk_stat' s in
						let t1, t2 = get_type e', get_type_stat s' in
							if t1 = TBoolean then
								If(e', s', t2)
							else
								If(e', s', TNone)
			
			| If_Else (e, s1, s2, _) ->
						let e', s1', s2' = typechk_exp' e, typechk_stat' s1, typechk_stat' s2 in
						let t1, t2, t3 = get_type e', get_type_stat s1', get_type_stat s2' in
							if t1 = TBoolean && t2 = TUnit && t3 = TUnit then
								If_Else(e', s1', s2', TUnit)
							else
								If_Else(e', s1', s2', TNone)
			
			| While (e, s, _) ->
						let e', s' = typechk_exp' e, typechk_stat' s in
						let t1, t2 = get_type e', get_type_stat s' in
							if t1 = TBoolean then
								While(e', s', t2)
							else
								While(e', s', TNone)
			
			| Write (list, _) -> 
						let list', write_type = 
							List.fold_left ( fun (prev_list, prev_type) e -> 
																		let e' = typechk_exp' e in
																		let t = get_type e' in
																			if t <> TNone && prev_type <> TNone then
																				(prev_list @ [e'], TUnit)
																			else
																				(prev_list @ [e'], TNone)
															) ([], TUnit) list in
							Write (list', write_type)
						
								
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
					let return_type_ref = ref t in
					let recursive_env = assoc name t temp_env in
					let new_env = assoc "result" (TRef(return_type_ref)) recursive_env in
					let s' = typechk_stat new_env s in
					let s_type = get_type_stat s' in
					let final_env = assoc name s_type temp_env in
						(name, Function (name, args_list, decl_block, s', s_type), final_env)
		
		| Procedure (name, args_list, [consts; vars; opers], s, _) -> 
					let decl_block, temp_env, decl_type = typechk_all_decls env args_list consts vars opers in
					let recursive_env = assoc name TUnit temp_env in
					let s' = typechk_stat recursive_env s in
					let s_type = get_type_stat s' in
					let final_env = assoc name s_type temp_env in
						(name, Procedure (name, args_list, decl_block, s', s_type), final_env)
		
		| _ -> raise (Invalid_type "") (* dummy *)

and typechk_decl env d =
		match d with	
			| Vars (list, _) -> 
      			let all_vars, vars_type, vars_env =
							List.fold_left (fun (prev_vars, prev_checked ,prev_env) (t, l) -> 
      														let temp_env = 
      															List.fold_left (fun prev s -> 
      																										assoc s t prev
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
  																			(prev_opers @ [name], prev_checked @ [oper'], TUnit, assoc name t prev_env)
  																		else
  																			(prev_opers @ [name], prev_checked @ [oper'], TNone, assoc name t prev_env)
      												) ([], [], TUnit, env) list in
							(all_opers, Operations(opers', opers_type), opers_env)

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
								
		| _ -> raise (Invalid_type "") (* dummy *)
