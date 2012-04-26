open Blaise_syntax
open Ivalue

exception IdNotFound of string
exception VariableAlreadyDeclared of string

let env = ref [];;
let resetEnv () = env := [];;
let assoc list = env := list@(!env);;

let find s =
	try
		List.assoc s !env
	with Not_found -> raise (IdNotFound s)
	
let rec containsConsts s list =
	match list with
		| [] -> false
		| (x, _)::xs -> if x = s then true
							 else containsConsts s xs	
						
let rec hasDuplicatesConsts list =
	match list with
		| [] -> ()
		| (s, _)::xs -> if containsConsts s xs then
											raise (VariableAlreadyDeclared s)
										else hasDuplicatesConsts xs
										
let rec containsVars s list =
	match list with
		| [] -> false
		| x::xs -> if x = s then true
							 else containsVars s xs	
						
let rec hasDuplicatesVars list =
	match list with
		| [] -> ()
		| s::xs -> if containsVars s xs then
											raise (VariableAlreadyDeclared s)
										else hasDuplicatesVars xs


let rec evalExp e =
		match e with
			| Number n -> NumberValue(n)
			| String s -> StringValue(s)
			| Boolean b -> BooleanValue(b)
			| Add (e1, e2) -> sum_ivalue (evalExp e1) (evalExp e2)
			| Sub (e1, e2) -> sub_ivalue (evalExp e1) (evalExp e2)
			| Mult (e1, e2) -> mult_ivalue (evalExp e1) (evalExp e2)
			| Div (e1, e2) -> div_ivalue (evalExp e1) (evalExp e2)
			| Compl e -> sub_ivalue (NumberValue(0)) (evalExp e)
			| Mod (e1, e2) -> mod_ivalue (evalExp e1) (evalExp e2)
			| Eq (e1, e2) -> eq_ivalue (evalExp e1) (evalExp e2)
			| Neq (e1, e2) -> let eq = eq_ivalue (evalExp e1) (evalExp e2) in
													not_ivalue eq
			| Gt (e1, e2) -> gt_ivalue (evalExp e1) (evalExp e2)
			| Lt (e1, e2) -> lt_ivalue (evalExp e1) (evalExp e2)
			| Gteq (e1, e2) -> let e1' = (evalExp e1) in
													let e2' = (evalExp e2) in
														let gt = gt_ivalue e1' e2' in
															let eq = eq_ivalue e1' e2' in
																or_ivalue gt eq
			| Lteq (e1, e2) -> let e1' = (evalExp e1) in
													let e2' = (evalExp e2) in
														let lt = lt_ivalue e1' e2' in
															let eq = eq_ivalue e1' e2' in
																or_ivalue lt eq
			| And (e1, e2) -> and_ivalue (evalExp e1) (evalExp e2)
			| Or (e1, e2) -> or_ivalue (evalExp e1) (evalExp e2)
			| Not e -> not_ivalue (evalExp e)
			| Id s -> find s

let rec evalState s =
	match s with
		| Assign (e1, e2) -> let (RefValue(r), e2') = (evalExp e1, evalExp e2) in
															r := e2'
		| While (e, s) -> let BooleanValue(b) = evalExp e in
												if b then (
													evalState s;
													evalState (While(e, s))
												) else 
													()
		| If_Else (e, s1, s2) -> let BooleanValue(b) = evalExp e in
															if b then (
																evalState s1
															) else
																evalState s2
		| If (e, s) -> let BooleanValue(b) = evalExp e in
										if b then (
											evalState s
										) else 
											()
		| Write list -> List.iter (fun e -> 
																	let s = string_of_ivalue (evalExp e) in
																		print_string s) list
		| WriteLn list -> evalState (Write(list)); print_string "\n"
		| Seq (s1, s2) -> evalState s1; evalState s2
		| Read list -> ()
		| ReadLn list -> ()

let rec evalDecls d =
	match d with
		| Consts (list) -> hasDuplicatesConsts list;
												let declarations = List.map (fun (x,y) -> (x, evalExp y)) list in
													assoc declarations
		| Vars (list) -> hasDuplicatesVars list;
												let declarations = List.map (fun x -> (x, RefValue(ref Uninitialized))) list in
													assoc declarations
(*		| Operations (list) -> let declarations = List.map (fun x -> (x, RefValue(ref Uninitialized))) list in*)
(*														assoc declarations                                                            *)
													
let rec evalProgram p =
	match p with
		| Program(name, [consts; vars; opers], s) -> evalDecls consts; evalDecls vars; evalDecls opers; evalState s
	 