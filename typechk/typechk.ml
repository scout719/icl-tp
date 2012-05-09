open Blaise_syntax


exception Invalid_type of string
exception Id_not_found of string

module EnvMap = Map.Make (String)


type env = string*iType EnvMap.t

let find s env =
	try
		EnvMap.find s env
	with Not_found -> raise (Id_not_found s)



let rec getType e =
		match e with
			| Number _ -> TNumber
			| String _ -> TString
			| Boolean _ -> TBoolean
			| Array(l,t) -> t 
			| Record(l,t) -> t
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



let bin_oper_int e1 e2 =
	let (t1,t2) = (getType e1, getType e2) in
		if t1 = TNumber && t2 = TNumber then true else false
	

let bin_oper_str e1 e2 =
	let (t1,t2) = (getType e1, getType e2) in
		if t1 = TString && t2 = TString then true else false


let bin_oper_bool e1 e2 =
	let (t1,t2) = (getType e1, getType e2) in
		if t1 = TBoolean && t2 = TBoolean then true else false	
	
let un_oper_int e =
	let t = getType e in
	if t = TNumber then true else false

let un_oper_str e =
	let t = getType e in
	if t = TString then true else false
	
let un_oper_bool e =
	let t = getType e in
	if t = TBoolean then true else false

let rec un_oper_array t =
	match t with
		| TRef r -> (un_oper_array !r)
		| TArray _ -> true
		| _ -> false
	
let type_of_array a = 
	match a with
		| TArray(_,t) -> t
		| _ -> raise (Invalid_type "")



let rec typechkExp env e =
	match e with
		| Number n -> Number(n)
		
		| String s -> String(s)
		
		| Boolean b -> Boolean(b)
		
		| Array(l,t) -> let size = List.length l in
										List.iter(fun next -> 
												let t' = getType next in
												if t' <> t 
												then raise (Invalid_type "array") ) l ; Array(l,TArray(size,t))
											
		| Record(l,t) -> let l' = List.fold_left(fun prev (s,e) -> (s,t)::prev) [] l in
												Record(l,TRecord(l'))
											
		| Add (e1, e2, _) -> let (e1',e2') = (typechkExp env e1,typechkExp env e2) in
													if (bin_oper_int e1' e2') then Add(e1', e2', TNumber)
													else if (bin_oper_str e1' e2') then Add(e1', e2', TString)
													else raise (Invalid_type "")
													
			
		| Sub (e1, e2, _) -> let (e1',e2') = (typechkExp env e1,typechkExp env e2) in
													if (bin_oper_int e1' e2') then Sub(e1', e2', TNumber)
													else raise (Invalid_type "")
													
		| Compl (e1, _) -> let e1' = typechkExp env e1 in
												if (un_oper_int e1') then Compl(e1', TNumber)
												else raise (Invalid_type "")
												
		| Mult (e1, e2, _) -> let (e1',e2') = (typechkExp env e1,typechkExp env e2) in
													if (bin_oper_int e1' e2') then Mult(e1', e2', TNumber)
													else raise (Invalid_type "")
													
		| Div (e1, e2, _) -> let (e1',e2') = (typechkExp env e1,typechkExp env e2) in
													if (bin_oper_int e1' e2') then Div(e1', e2', TNumber)
													else raise (Invalid_type "")
													
		| Mod (e1, e2, _) -> let (e1',e2') = (typechkExp env e1,typechkExp env e2) in
													if (bin_oper_int e1' e2') then Mod(e1', e2', TNumber)
													else raise (Invalid_type "")
													
		| Eq (e1, e2, _) -> let (e1',e2') = (typechkExp env e1,typechkExp env e2) in
													if (bin_oper_int e1' e2') || (bin_oper_str e1' e2')
													then Eq(e1',e2',TBoolean)
													else raise (Invalid_type "")
													
		| Neq (e1, e2, _) -> let (e1',e2') = (typechkExp env e1,typechkExp env e2) in
													if (bin_oper_int e1' e2') || (bin_oper_str e1' e2')
													then Neq(e1',e2',TBoolean)
													else raise (Invalid_type "")
													
		| Gt (e1, e2, _) -> let (e1',e2') = (typechkExp env e1,typechkExp env e2) in
													if (bin_oper_int e1' e2') || (bin_oper_str e1' e2')
													then Gt(e1',e2',TBoolean)
													else raise (Invalid_type "")
													
		| Lt (e1, e2, _) -> let (e1',e2') = (typechkExp env e1,typechkExp env e2) in
													if (bin_oper_int e1' e2') || (bin_oper_str e1' e2')
													then Lt(e1',e2',TBoolean)
													else raise (Invalid_type "")
													
		| Gteq (e1, e2, _) -> let (e1',e2') = (typechkExp env e1,typechkExp env e2) in
													if (bin_oper_int e1' e2') || (bin_oper_str e1' e2')
													then Gteq(e1',e2',TBoolean)
													else raise (Invalid_type "")
													
		| Lteq (e1, e2, _) -> let (e1',e2') = (typechkExp env e1,typechkExp env e2) in
													if (bin_oper_int e1' e2') || (bin_oper_str e1' e2')
													then Lteq(e1',e2',TBoolean)
													else raise (Invalid_type "")
													
		| And (e1, e2, _) -> let (e1',e2') = (typechkExp env e1,typechkExp env e2) in
													if (bin_oper_str e1' e2') then And(e1',e2',TBoolean)
													else raise (Invalid_type "")
													
		| Or (e1, e2, _) -> let (e1',e2') = (typechkExp env e1,typechkExp env e2) in
													if (bin_oper_int e1' e2') then Or(e1',e2',TBoolean)
													else raise (Invalid_type "")
													
		| Not (e1, _) -> let e1' = typechkExp env e1 in
											if (un_oper_bool e1') then Not(e1',TBoolean)
											else raise (Invalid_type "")
											
		| Id (s, _) -> Id(s,getType(find s env))
		
		| GetArray(e1, e2, _) -> let(e1',e2') = (typechkExp env e1,typechkExp env e2) in 
															let t1 = getType e1' in
																if (un_oper_array t1) && (un_oper_int e2') 
																then( let ty_array = type_of_array t1 in GetArray(e1',e2',ty_array))
																else raise (Invalid_type "")
									
(*		| GetRecord (e1, s, t) -> *)
(*		| CallFun (e1,l,t) ->     *)
