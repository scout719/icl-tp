open Blaise_syntax
open Ivalue

exception Id_not_found of string
exception Id_found of string
exception Variable_already_declared of string
exception Constant_already_declared of string
exception Element_not_found_in_record of string
exception Index_out_of_bounds of int

let find s env =
	try
		List.assoc s env
	with Not_found -> raise (Id_not_found s)
	
let assoc list env =
	list@env
						
let hasDuplicatesConsts list =
	try
		List.fold_left (fun aux (s, _) -> 
																	try
																		List.find (fun v -> v = s) aux;
																		raise (Id_found s)
																	with
																		| Not_found ->  s::aux 
																) [] list; ()
	with
	| Id_found s -> raise (Constant_already_declared s)

let hasDuplicatesVars list =
	try
		List.fold_left (fun aux (s) -> 
																	try
																		List.find (fun v -> v = s) aux;
																		raise (Id_found s)
																	with
																		| Not_found ->  s::aux 
																) [] list; ()
	with
	| Id_found s -> raise (Variable_already_declared s)

let rec evalExp env e =
	let evalExp' = evalExp env in
			match e with
			| Number n -> NumberValue(n)
			| String s -> StringValue(s)
			| Boolean b -> BooleanValue(b)
			| Record list -> let map = List.fold_left (fun prev (s, e) -> RecordMap.add s (evalExp' e) prev) RecordMap.empty list in
												RecordValue(map)
			| Array (list) -> let list2 = List.map (fun e -> evalExp' e) list in
													ArrayValue( Array.of_list list2)
			| Add (e1, e2) -> sum_ivalue (evalExp' e1) (evalExp' e2)
			| Sub (e1, e2) -> sub_ivalue (evalExp' e1) (evalExp' e2)
			| Mult (e1, e2) -> mult_ivalue (evalExp' e1) (evalExp' e2)
			| Div (e1, e2) -> div_ivalue (evalExp' e1) (evalExp' e2)
			| Compl e -> sub_ivalue (NumberValue(0)) (evalExp' e)
			| Mod (e1, e2) -> mod_ivalue (evalExp' e1) (evalExp' e2)
			| Eq (e1, e2) -> eq_ivalue (evalExp' e1) (evalExp' e2)
			| Neq (e1, e2) -> let eq = eq_ivalue (evalExp' e1) (evalExp' e2) in
													not_ivalue eq
			| Gt (e1, e2) -> gt_ivalue (evalExp' e1) (evalExp' e2)
			| Lt (e1, e2) -> lt_ivalue (evalExp' e1) (evalExp' e2)
			| Gteq (e1, e2) -> let e1' = (evalExp' e1) in
													let e2' = (evalExp' e2) in
														let gt = gt_ivalue e1' e2' in
															let eq = eq_ivalue e1' e2' in
																or_ivalue gt eq
			| Lteq (e1, e2) -> let e1' = (evalExp' e1) in
													let e2' = (evalExp' e2) in
														let lt = lt_ivalue e1' e2' in
															let eq = eq_ivalue e1' e2' in
																or_ivalue lt eq
			| And (e1, e2) -> and_ivalue (evalExp' e1) (evalExp' e2)
			| Or (e1, e2) -> or_ivalue (evalExp' e1) (evalExp' e2)
			| Not e -> not_ivalue (evalExp' e)
			| Id s -> find s env
			| GetRecord (e, s) -> (try 
															get_record_ivalue (evalExp' e) s
														with
															| Not_found -> raise (Element_not_found_in_record s)
														)
			| GetArray (e1, e2) -> get_array_ivalue (evalExp' e1) (evalExp' e2)
			| CallFun (e, list) -> let FunValue(listArgs, s, _, closure_env) = evalExp' e in
																	let args_env = List.map2 (fun (s, _) e1 -> (s, evalExp' e1)) listArgs list in
																		let new_env = closure_env@args_env@env in
																			let result_env = evalState new_env s in
																				find "result" result_env

and evalState env s =
	let evalState' = evalState env in
		let evalExp' = evalExp env in
			match s with
				| Assign (e1, e2) -> (match e1 with
																| Id("result") -> assoc [("result", (evalExp' e2))] env
																| _ -> let (RefValue(r), e2') = (evalExp' e1, evalExp' e2) in
																					r := e2';
																					env
															)
				| While (e, s) -> let BooleanValue(b) = evalExp' e in
														if b then (
															evalState' s;
															evalState' (While(e, s))
														) else
															env
				| If_Else (e, s1, s2) -> let BooleanValue(b) = evalExp' e in
																	if b then (
																		evalState' s1;
																		env
																	) else
																		evalState' s2;
																		env
				| If (e, s) -> let BooleanValue(b) = evalExp' e in
												if b then (
													evalState' s;
													env
												) else 
													env
				| Write list -> List.iter (fun e -> 
																			let s = string_of_ivalue (evalExp' e) in
																				print_string s) list;
																				env
				| WriteLn list -> evalState' (Write(list));
													print_string "\n";
													env
				| Seq (s1, s2) -> let env1 = evalState' s1 in
														let env2 = evalState env1 s2 in
															env1@env2
				| Read list -> env
				| ReadLn list -> env
				| CallProc (e, list) -> let ProcValue(listArgs, s, closure_env) = evalExp' e in
																	let args_env = List.map2 (fun (s, _) e1 -> (s, evalExp' e1)) listArgs list in
																		let new_env = closure_env@args_env@env in
																			evalState new_env s;
																			env

let rec evalOpers env o =
	let evalDecls' = evalDecls env in
		match o with
			| Function(name, listArgs, [consts; vars; opers], s, t) -> let env_consts = evalDecls' consts in
																																		let env_vars = evalDecls' vars in
																																			let env_opers = evalDecls' opers in
																																				(name, FunValue(listArgs, s, t, env_consts@env_vars@env_opers))
			| Procedure(name, listArgs, [consts; vars; opers], s) -> let env_consts = evalDecls' consts in
																																		let env_vars = evalDecls' vars in
																																			let env_opers = evalDecls' opers in
																																				(name, ProcValue(listArgs, s, env_consts@env_vars@env_opers))
			| _ -> ("", None) (* dummy *)
			
and evalDecls env d =
	let evalExp' = evalExp env in
		let evalOpers' = evalOpers env in
		match d with
			| Consts (list) -> hasDuplicatesConsts list;
													let declarations = List.map (fun (x,y) -> (x, evalExp' y)) list in
														assoc declarations env
			| Vars (list) -> let allVars = List.fold_left (fun prev (_, l) -> prev@l ) [] list in
													hasDuplicatesVars allVars;
														let declarations = List.fold_left (fun prev (t, l) -> 
																											let tempDecl = List.map (fun s -> (s, RefValue(ref (defaultValue t)))) l in
																												prev@tempDecl
																							) [] list in
															assoc declarations env
			| Operations (list) -> let declarations = List.map (fun x -> evalOpers' x) list in
															assoc declarations env
													
let rec evalProgram p =
	match p with
		| Program(name, [consts; vars; opers], s) -> let env_consts = evalDecls [] consts in
																									let env_vars = evalDecls [] vars in
																										let env_opers = evalDecls [] opers in
																											evalState (env_consts@env_vars@env_opers) s;
																											()
		| _ -> () (* dummy *)
