open Blaise_syntax
open Ivalue
(*oiiiiiiiiiiiii*)
exception IdNotFound of string

let find s env =
	try
		List.assoc s env
	with Not_found -> raise (IdNotFound s)

let rec evalExp env e =
	let rec evalExp' = evalExp env in
		match e with
			| Number n -> NumberValue(n)
			| String s -> StringValue(s)
			| Boolean b -> BooleanValue(b)
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

let rec evalState env s =
	let evalState' = evalState env in
		let evalExp' = evalExp env in
			match s with
(*				| Assign (e1, e2) -> assign_ivalue (evalExp' e1) (evalExp' e2) env*)
				| While (e, s) -> let BooleanValue(b) = evalExp' e in
														if b then (
															evalState' s;
															evalState' (While(e, s))
														) else 
															()
				| If_Else (e, s1, s2) -> let BooleanValue(b) = evalExp' e in
																	if b then (
																		evalState' s1
																	) else
																		evalState' s2
				| If (e, s) -> let BooleanValue(b) = evalExp' e in
												if b then (
													evalState' s
												) else 
													()
				| Write list -> List.iter (fun e -> 
																			let s = string_of_ivalue (evalExp' e) in
																				print_string s) list
				| WriteLn list -> evalState' (Write(list)); print_string "\n"
				| Seq (s1, s2) -> evalState' s1; evalState' s2 
