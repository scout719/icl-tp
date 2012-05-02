open Blaise_syntax
open Ivalue

exception Id_not_found of string
exception Id_found of string
exception Variable_already_declared of string
exception Constant_already_declared of string
exception Element_not_found_in_record of string
exception Index_out_of_bounds of int

let buffer = ref [];;

let clearBuffer () = buffer := [];;

let cut s =
	(String.get s 0, String.sub s 1 ((String.length s) - 1));;

let rec split s = (* parte a string s no primeiro ' ' *)
	if s = "" then ("", "") (* caso base *)
	else
		let (x, xs) = cut s in (* separa cabeca da cauda *)
			if x = ' ' then ("", xs) (* outro caso base *)
			else let (a, b) = split xs in (* chamada recursiva para a cauda *)
				let s = " " in
					s.[0] <- x; (* converter o char para string *)
					( s^a, b);;

let rec readNext () =
	if List.length !buffer <> 0 then (
		let x::xs = !buffer in
			let (next, rest) = split x in 
				if rest <> "" then (
					buffer := rest::xs;
				) else (
					buffer := xs;
				);
				if next <> "\n" then(
					next
				) else (
					readNext()
				)
	) else (
		let line = read_line() in
			buffer := [line];
			readNext()
	);;

let rec readLine () =
	let (buf, _) = List.fold_left (fun (prevBuf, prevFound) s -> if not prevFound then (
																																([], s = "\n")
																															) else (
																																(prevBuf@[s], true)
																															)
									) ([], false) !buffer in
				buffer := buf;;

let find s env =
	try
		List.assoc s env
	with Not_found -> raise (Id_not_found s)
	
let assoc k v env =
	(k,v)::env
						
let hasDuplicatesConsts list =
	List.iter (fun (x, _) -> let all = List.find_all (fun (s, _) -> s = x) list in
														if List.length all <> 1 then
															raise (Constant_already_declared x)
														else
															()) list
	
let hasDuplicatesVars list =
	List.iter (fun x -> let all = List.find_all (fun s -> s = x) list in
														if List.length all <> 1 then
															raise (Variable_already_declared x)
														else
															()) list

let value_from_string_type t s =
	match t with
		| NumberValue _ -> NumberValue(int_of_string s)
		| StringValue _ -> StringValue(s)
		| BooleanValue _ -> BooleanValue(bool_of_string s)
															
let rec toresult lvalue value =
		match value with
			| RefValue(r) -> if not lvalue then (
												toresult lvalue !r
											) else (
												value
											)
			| _ -> value
let rec evalExp env lvalue e =
	let toresult' = toresult lvalue in
		let evalExp' = evalExp env false in
				match e with
				| Number n -> NumberValue(n)
				| String s -> StringValue(s)
				| Boolean b -> BooleanValue(b)
				| Record list -> let map = List.fold_left (fun prev (s, e) -> RecordMap.add s (evalExp' e) prev) RecordMap.empty list in
													RecordValue(map)
				| Array (list) -> let list2 = List.map (fun e -> evalExp' e) list in
														ArrayValue( Array.of_list list2)
				| Add (e1, e2) -> sum_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
				| Sub (e1, e2) -> sub_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
				| Mult (e1, e2) -> mult_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
				| Div (e1, e2) -> div_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
				| Compl e -> sub_ivalue (NumberValue(0)) (toresult' (evalExp' e))
				| Mod (e1, e2) -> mod_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
				| Eq (e1, e2) -> eq_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
				| Neq (e1, e2) -> let eq = eq_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2)) in
														not_ivalue eq
				| Gt (e1, e2) -> gt_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
				| Lt (e1, e2) -> lt_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
				| Gteq (e1, e2) -> let e1' = (toresult' (evalExp' e1)) in
														let e2' = (toresult' (evalExp' e2)) in
															let gt = gt_ivalue e1' e2' in
																let eq = eq_ivalue e1' e2' in
																	or_ivalue gt eq
				| Lteq (e1, e2) -> let e1' = (toresult' (evalExp' e1)) in
														let e2' = (toresult' (evalExp' e2)) in
															let lt = lt_ivalue e1' e2' in
																let eq = eq_ivalue e1' e2' in
																	or_ivalue lt eq
				| And (e1, e2) -> and_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
				| Or (e1, e2) -> or_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
				| Not e -> not_ivalue (toresult' (evalExp' e))
				| Id s -> toresult' (find s env)
				| GetRecord (e, s) -> (try 
																toresult' (get_record_ivalue (evalExp' e) s)
															with
																| Not_found -> raise (Element_not_found_in_record s)
															)
				| GetArray (e1, e2) -> toresult' (get_array_ivalue (evalExp' e1) (evalExp' e2))
				| CallFun (e, list) -> let FunValue(listArgs, [consts;vars;opers], s, t, closure_env) = evalExp' e in
																let args_env = List.map2 (fun (s, _) e1 -> (s, evalExp' e1)) listArgs list in
																	let env_consts = evalDecls (args_env@closure_env) consts in
																		let env_vars = evalDecls env_consts vars in
																			let env_opers = evalDecls env_vars opers in
																				let new_env = assoc "result" (RefValue(ref (defaultValue t))) (env_opers) in
																					let result_env = evalState (new_env@env) s in
																						toresult' (find "result" result_env)

and evalState env s =
	let evalState' = evalState env in
		let evalExp' = evalExp env false in
			match s with
				| Assign (e1, e2) -> let (RefValue(r), e2') = (evalExp env true e1, evalExp' e2) in
															r := e2';
															env
				| While (e, s) -> let BooleanValue(b) = evalExp' e in
														if b then (
															let temp_env = evalState' s in
																let new_node = While(e,s) in
																	evalState temp_env new_node
														) else
															env
				| If_Else (e, s1, s2) -> let BooleanValue(b) = evalExp' e in
																	if b then (
																		evalState' s1
																	) else (
																		evalState' s2
																	)
				| If (e, s) -> let BooleanValue(b) = evalExp' e in
												if b then (
													evalState' s
												) else 
													env
				| Write list -> List.iter (fun e -> 
																			let s = string_of_ivalue (evalExp' e) in
																				print_string s) list;
																				env
				| WriteLn list -> evalState' (Write list);
													print_string "\n";
													env
				| Seq (s1, s2) -> let env1 = evalState' s1 in
														evalState env1 s2
				| Read list -> List.iter (fun s -> let RefValue r = find s env in 
																						let value = value_from_string_type !r (readNext()) in
																							r := value
																	) list; env
				| ReadLn list -> let new_env = evalState' (Read list) in
														readLine();
														new_env 
				| CallProc (e, list) -> let ProcValue(listArgs, [consts;vars;opers], s, closure_env) = evalExp' e in
																	let args_env = List.map2 (fun (s, _) e1 -> (s, evalExp' e1)) listArgs list in
																		let env_consts = evalDecls (args_env@closure_env) consts in
																			let env_vars = evalDecls env_consts vars in
																				let env_opers = evalDecls env_vars opers in
																						evalState (env_opers@env) s;
																						env (* nao se retorna o env do proc para nao vir com as consts, vars, args e env do proc *)

and evalOpers env o =
		match o with
			| Function(name, listArgs, decl, s, t) -> assoc name (FunValue(listArgs, decl, s, t, env)) env
			| Procedure(name, listArgs, decl, s) -> assoc name (ProcValue(listArgs, decl, s, env)) env
			| _ -> [] (* dummy *)
			
and evalDecls env d = (* ATENTION  verificar se as variaveis nao colidem com as constantes*)
	let evalExp' = evalExp env false in
		match d with
			(* Verificar se nao existem duplicados e caso nao exista percorrer todas as declaracoes e criar o novo *)
			(* ambiente *)
			| Consts (list) -> hasDuplicatesConsts list;
													List.fold_left (fun prev_env (x,y) -> assoc x (evalExp prev_env false y) prev_env) env list
			(* Percorrer as varias listas e criar uma lista com todas as variaveis e criar o novo ambiente, depois verificar *)
			(* se nao ha duplicados e se nao houver retornar o ambiente com as variaveis inicializadas *)
			| Vars (list) -> let (new_env, allVars) = List.fold_left (fun (prev_env, prev_vars) (t, l) -> 
																									let temp_env = List.fold_left (fun prev s -> assoc s (RefValue(ref (defaultValue t))) prev) prev_env l in
																										(temp_env@prev_env, l@prev_vars)
																					) (env, []) list in
													hasDuplicatesVars allVars;
														new_env
			(* Percorrer todas as declaracoes de funcoes e procedimentos e avaliar cada declaracao e retornar o ambiente *)
			| Operations (list) -> List.fold_left (fun prev_env x -> (evalOpers prev_env x)) env list
													
let rec evalProgram p =
	clearBuffer();
	match p with
		(* Avaliar cada parte do bloco das declaracoes, juntar tudo num env e enviar para a avaliacao *)
		(* do corpo principal do programa *)
		| Program(name, [consts; vars; opers], s) -> let env_consts = evalDecls [] consts in
																									let env_vars = evalDecls [] vars in
																										let env_opers = evalDecls (env_vars@env_consts) opers in
																											evalState env_opers s;
																											()
		| _ -> () (* dummy *)
