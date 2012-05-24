open Blaise_syntax
open Blaise_typechk
open Ivalue
open Blaise_iType

exception Id_not_found of string
exception Id_found of string
exception Element_not_found_in_record of string
exception Index_out_of_bounds
exception Invalid_value_to_read of string
exception Invalid_value of string

let buffer = ref [];;

let clearBuffer () = buffer := [];;

(* Funcao que parte uma string pelos espacos numa lista de strings contendo *)
(* os varios tokens *)
(* (retirada de http://rosettacode.org/wiki/Tokenize_a_string#OCaml) *)
let split_char sep str =
  let string_index_from i =
    try Some (String.index_from str i sep)
    with Not_found -> None
  in
  let rec aux i acc = match string_index_from i with
    | Some i' ->
        let w = String.sub str i (i' - i) in
        aux (succ i') (w::acc)
    | None ->
        let w = String.sub str i (String.length str - i) in
        List.rev (w::acc)
  in
  aux 0 []

(* Funcao que le o proximo token do input e caso nao haja nada no input fica *)
(* pendurado *)
let rec readNext () =
	if List.length !buffer <> 0 then (
		let next = List.hd !buffer in
			buffer := List.tl !buffer;
			next
	) else (
		let line = split_char ' ' (read_line()) in
			buffer := line;
			readNext()
	);;

(* Funcao que retorna o valor do id s em env desreferenciando-o *)
(* caso o id nao exista lanca Id_not_found s *)
let find s env =
	try
		EnvMap.find s env
	with Not_found -> raise (Id_not_found s)
	
(* Funcao que adiciona ao env um tuplo com chave k e valor v *)
let assoc k v env =
	EnvMap.add k v env

(* Funcao que consoante o valor passado em v converte o valor da string s *)
(* para o valor correcto *)
let value_from_string v s =
	match v with
		| NumberValue _ -> NumberValue(int_of_string s)
		| StringValue _ -> StringValue(s)
		| BooleanValue _ -> BooleanValue(bool_of_string s)
		| _ -> raise (Invalid_value_to_read (string_of_ivalue v))
		
(* Funcao que conforme o parametro lvalue desreferencia implicitamente ou *)
(* nao o valor value *)	
let toresult lvalue value =
		match value with
			| RefValue r -> 
					if not lvalue then
						!r
					else
						value
					
			| _ -> value

(* Funcao auxiliar que faz o assign a variavel lvalue com o valor rvalue *)
(* Se os valores forem records ou arrays faz assign dos valores campo a *)
(* campo ou indice a indice *)
let rec assign lvalue rvalue =
	match lvalue with
	| RefValue r -> 
			(match !r, rvalue with
			| ArrayValue array, ArrayValue array2 -> 
					Array.iteri (fun index elem -> 
													let rValElem = toresult false (Array.get array2 index) in
														assign elem rValElem; ()
											) array
												
			| RecordValue record, RecordValue record2 -> 
					RecordMap.iter (fun k v -> 
														let rValElem = toresult false (RecordMap.find k record2) in
															assign v rValElem; ()
													) record 
														
			| NumberValue _, NumberValue _ -> r := rvalue
			| StringValue _, StringValue _ -> r := rvalue
			| BooleanValue _, BooleanValue _ -> r := rvalue
			| FunValue _ , FunValue _ -> r := rvalue
			| ProcValue _, ProcValue _ -> r := rvalue

			(* no caso de ser a primeira afectacao o valor por default da fun e none *)
			| NoneValue, FunValue _ -> r := rvalue
			
			(* no caso de ser a primeira afectacao o valor por default do proc e none *)
			| NoneValue, ProcValue _ -> r := rvalue
			| _ -> raise (Invalid_value ("Values don't match in assign: "^(string_of_ivalue lvalue) ^ " " ^(string_of_ivalue rvalue)))
			)
			
	| _ -> raise (Invalid_value ("Ref expected: "^(string_of_ivalue lvalue)))
				
let get_oper_info oper = 
	match oper with
		| Function(name, list, _, _, t) -> 
					let args_types_list = List.fold_left (fun prev_list (_, t) ->
																											prev_list @ [t]
																								) [] list in
						(name, TFun(args_types_list, t))

		| Procedure(name, list, _, _, t) -> 
					let args_types_list = List.fold_left (fun prev_list (_, t) ->
																											prev_list @ [t]
																								) [] list in
						(name, TProc(args_types_list))


let get_methods opers = 
	List.fold_left (fun prev_list oper ->
				let oper_info = get_oper_info oper in
					prev_list @ [oper_info]
									) [] opers

let get_self_record self_type =
	match self_type with
		| TRecord list -> 
					let new_list = List.fold_left (fun prev_list (s, t) ->
								prev_list @ [(s, Id(s, t))]
																				) [] list in
						Record(new_list, self_type)
		| _ -> Record([], self_type) (* dummy *)

(***************************************************************************)
(************************    INTERPRETER EXPR    ***************************)
(***************************************************************************)


(* Funcao que avalia uma expressao no env dado e desreferencia implicitamente *)
(* se lvalue for false *)
let rec evalExp env lvalue e =
	let toresult' = toresult lvalue in
	let evalExp' = evalExp env false in
		match e with
		| Number n -> NumberValue(n)
		| String s -> StringValue(s)
		| Boolean b -> BooleanValue(b)
		
		| Record (list,_) -> 
				let map = List.fold_left (fun prev (s, e) -> 
																				RecordMap.add s (evalExp' e) prev
																	) RecordMap.empty list in
						RecordValue(map)
		
		| Array (list,_) -> 
				let list2 = List.map (fun e -> 
																	evalExp' e
															) list in
						ArrayValue( Array.of_list list2)

		| New (e, t) ->
				evalExp' (CallFun(e, [], t))

		| Add (e1, e2,_) -> 
				sum_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
		
		| Sub (e1, e2,_) -> 
				sub_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
		
		| Mult (e1, e2,_) -> 
				mult_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
		
		| Div (e1, e2,_) -> 
				div_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
		
		| Compl (e,_) -> 
				sub_ivalue (NumberValue(0)) (toresult' (evalExp' e))
		
		| Mod (e1, e2,_) -> 
				mod_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
		
		| Eq (e1, e2,_) -> 
				eq_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
		
		| Neq (e1, e2,_) -> 
				let eq = eq_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2)) in
					not_ivalue eq
		
		| Gt (e1, e2,_) -> 
				gt_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
		
		| Lt (e1, e2,_) -> 
				lt_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
		
		| Gteq (e1, e2,_) -> 
				let e1' = (toresult' (evalExp' e1)) in
				let e2' = (toresult' (evalExp' e2)) in
				let gt = gt_ivalue e1' e2' in
				let eq = eq_ivalue e1' e2' in
					or_ivalue gt eq
		
		| Lteq (e1, e2,_) -> 
				let e1' = (toresult' (evalExp' e1)) in
				let e2' = (toresult' (evalExp' e2)) in
				let lt = lt_ivalue e1' e2' in
				let eq = eq_ivalue e1' e2' in
					or_ivalue lt eq
		
		| And (e1, e2,_) -> 
				and_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
		
		| Or (e1, e2,_) -> 
				or_ivalue (toresult' (evalExp' e1)) (toresult' (evalExp' e2))
		
		| Not (e,_) -> not_ivalue (toresult' (evalExp' e))
		| Id (s,_) -> toresult' (find s env)
		
		| GetRecord (e, s,_) -> 
				(try 
					toresult' (get_record_ivalue (evalExp' e) s)
				with
					| Not_found -> raise (Element_not_found_in_record s)
				)
		
		| GetArray (e1, e2,_) -> 
				(try
					toresult' (get_array_ivalue (evalExp' e1) (evalExp' e2))
				with
					| Invalid_argument _ -> raise (Index_out_of_bounds)
				)
				
		| CallFun (e, list,_) -> 
				let exp = evalExp' e in
				(match exp with
				| FunValue(listArgs, [consts;vars;opers], s, t, closure_env) -> 
          	(* criar args *)
          	let args_env = 
          		List.fold_left2 (fun prev_env (s, _) e1 -> 
          													let param = evalExp' e1 in
          														assoc s param prev_env
          										) !closure_env listArgs list in
						let temp_env = evalAllDecls consts vars opers args_env in
						(* preparar env para ter uma var *)
						(* result para guardar o resultado da funcao *)
						let new_env = assoc "result" (RefValue(ref (defaultValue t))) temp_env in 
							(* avaliar funcao *)
							evalState new_env s;
							(* ir buscar o valor ao env *)
							toresult' (find "result" new_env)
												
				| _ -> raise (Invalid_value ("FunValue expected: "^(string_of_ivalue exp)))
				)


(***************************************************************************)
(************************     INTERPRETER STATE   **************************)
(***************************************************************************)


(* Funcao que avalia um statement num dado env e retorna o env resultante *)
and evalState env s =
	let evalState' = evalState env in
	let evalExp' = evalExp env false in
		match s with
		| Assign (e1, e2, _) -> 
				let e1', e2' = evalExp env true e1, evalExp' e2 in
					assign e1' e2'
						
		| While (e, s, t) -> 
				let exp = evalExp' e in
					(match exp with
						| BooleanValue b -> if b then (
																	evalState' s;
																	let new_node = While(e, s, t) in
																		evalState' new_node
																) else
																		()
						| _ -> raise (Invalid_value ("Boolean expected: "^(string_of_ivalue exp)))
					)
					
		| If_Else (e, s1, s2, _) -> 
				let exp = evalExp' e in
					(match exp with
						| BooleanValue b -> if b then (
																	evalState' s1
																) else (
																	evalState' s2
																)
						| _ -> raise (Invalid_value ("Boolean expected: "^(string_of_ivalue exp)))
					)
					
		| If (e, s, _) -> 
				let exp = evalExp' e in
					(match exp with
						| BooleanValue(b) -> if b then (
																	evalState' s
																) else 
																	()
						| _ -> raise (Invalid_value ("Boolean expected: "^(string_of_ivalue exp)))
					)
					
		| Write (list, _) -> 
				List.iter (fun e -> 
											let s = string_of_ivalue (evalExp' e) in
												print_string s) list;
												()
		
		| WriteLn (list, t) -> 
				evalState' (Write (list, t));
				print_string "\n";
				flush stdout
		
		| Seq (s1, s2, _) -> 
				evalState' s1;
				evalState' s2
		
		| Read (list, _, _) -> 
				List.iter (fun s -> let found = find s env in
											(match found with
												| RefValue r -> 
														let value = value_from_string !r (readNext()) in
															r := value
												| _ -> raise (Invalid_value ("Ref expected: "^(string_of_ivalue found)))
											)
									) list
		
		| ReadLn (list, tl, t) -> 
				evalState' (Read (list, tl, t));
				clearBuffer()
					
		| CallProc (e, list, _) -> 
				let exp = evalExp' e in
				(match exp with
  				| ProcValue(listArgs, [consts;vars;opers], s, closure_env) -> 
            	(* criar args *)
            	let args_env = 
            		List.fold_left2 (fun prev_env (s, _) e1 -> 
            													let param = evalExp' e1 in
            														assoc s param prev_env
            										) !closure_env listArgs list in
									
						let new_env = evalAllDecls consts vars opers args_env in
							(* avaliar procedimento *)
							evalState new_env s
  												
  				| _ -> raise (Invalid_value ("Proc expected: "^(string_of_ivalue exp)))
				)


(***************************************************************************)
(************************     INTERPRETER OPERS   **************************)
(***************************************************************************)


(* Funcao que avalia a declaracao de uma operacao e retorna o env *)
(* actualizado com o closure *)
and evalOpers env o =
	match o with
	| Function(name, listArgs, decl, s, t) -> 
			let ref_env = ref env in
			let closure = FunValue(listArgs, decl, s, t, ref_env) in
			let new_env = assoc name closure env in
				ref_env := new_env;
				new_env

	| Procedure(name, listArgs, decl, s, _) ->  
			let ref_env = ref env in
			let closure = ProcValue(listArgs, decl, s, ref_env) in
			let new_env = assoc name closure env in
				ref_env := new_env;
				new_env

	| Class (name, [consts; Vars vars; Operations(opers, t1)], statement, t) ->
			let method_list = get_methods opers in
			let self_type = TRecord(method_list) in
			let self_record = get_self_record self_type in
			let new_vars = vars @ [(self_type, ["self"])] in
			let new_statement = 
				Seq(
					statement, 
					Seq(
							Assign(
									Id("self", TRef(ref self_type)), 
									self_record, 
									TUnit), 
							Assign(
									Id("result", TRef(ref self_type)), 
									Id("self", TRef(ref self_type)), 
									TUnit), 
							TUnit), 
					TUnit) in
			let new_function = Function(name, [], [consts; Vars new_vars; Operations(opers, t1)], new_statement, self_type) in
				evalOpers env new_function

	| _ -> env (* dummy *)

(***************************************************************************)
(************************     INTERPRETER DECLS  ***************************)
(***************************************************************************)


(* Funcao que retorna o env actualizado com as varias declaracoes *)
(* Esta funcao verifica se existe variaveis e constantes repetidas *)
(* lancando uma excepcao quando isso aconteca *)
and evalAllDecls consts vars opers env =
		(* criar consts *)
		let env_consts = evalDecls env consts in
		(* criar vars *)
		let env_vars = evalDecls env_consts vars in
		(* criar opers *)
		let env_opers = evalDecls env_vars opers in
  		env_opers

(* Funcao que avalia um bloco de declaracoes e retorna um env actualizado *)
(* com as declaracoes *)
and evalDecls env d = 
	match d with
	(* Verificar se nao existem duplicados e caso nao exista percorrer todas *)
	(* as declaracoes e criar o novo ambiente *)
	| Consts (list, _) -> 
			List.fold_left (fun prev_env (x,y) -> 
														assoc x (evalExp prev_env false y) prev_env
											) env list
												
	(* Percorrer as varias listas e criar uma lista com todas as variaveis e *)
	(* criar o novo ambiente, depois verificar se nao ha duplicados e se nao *)
	(* houver retornar o ambiente com as variaveis inicializadas *)
	| Vars list -> 
			List.fold_left (fun prev_env (t, l) -> 
														let temp_env = 
																List.fold_left (fun prev s -> 
																										assoc s (RefValue(ref (defaultValue t))) prev
																								) prev_env l in
															temp_env
											) env list

	(* Percorrer todas as declaracoes de funcoes e procedimentos e avaliar *)
	(* cada declaracao e retornar o ambiente *)
	| Operations (list, _) -> 
			List.fold_left (fun prev_env x ->
													evalOpers prev_env x
											) env list


(***************************************************************************)
(************************     INTERPRETER PROG   ***************************)
(***************************************************************************)


(* Funcao que avalia um programa avaliando as declaracoes num novo ambiente *)
(* e executa as instrucoes do corpo do programa *)
let evalProgram p =
	clearBuffer();
		match p with
			(* Avaliar cada parte do bloco das declaracoes, juntar tudo num env e *)
			(* enviar para a avaliacao do corpo principal do programa *)
			| Program(name, [consts; vars; opers], s, _) -> 
					let env = evalAllDecls consts vars opers EnvMap.empty in
					let _ = evalState env s in
						()
									
			| _ -> () (* dummy *)
