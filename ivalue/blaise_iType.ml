module MyMap = Map.Make (String);;

type iType =
	| TNumber
	| TString
	| TBoolean
	| TFun of (iType list) * iType
	| TProc of (iType list)
	| TArray of int * iType
	| TRecord of (string * iType) list
	| TRef of iType ref
	| TClass of string * (string * iType) list
	| TObject of string * (string * iType) list
	| TClass_id of string
	| TUnit
	| TNone of string
	| TUndefined;;

let rec unref_iType t =
	match t with
		| TRef r -> unref_iType !r
		| t -> t;;

let is_var t =
	match t with
		| TRef _ -> true
		| _ -> false;;

let rec unfold_type env t =
	match t with
		| TClass_id s ->
					( try
        		unfold_type env (MyMap.find s env)
        	with Not_found -> TNone "Type not found"
					)

		(* | _ -> t;; *)
		| TNumber -> t
  	| TString -> t
  	| TBoolean -> t

  	| TFun (list, t) -> 
					let params_list = List.map (fun t ->
																					unfold_type env t
  																		) list in
						TFun(params_list, unfold_type env t)
		
  	| TProc list -> 
					let params_list = List.map (fun t ->
																					unfold_type env t
																			) list in
						TProc(params_list)
		
  	| TArray (size, t) -> 
					TArray( size, unfold_type env t)
  	
		| TRecord list -> 
					let fields_list = List.map ( fun 	(s, t) ->
																						(s,unfold_type env t)
																		) list in
						TRecord(fields_list)
  	
		| TRef r -> TRef(ref (unfold_type env t))
		
		| TObject (name, list) ->
					let methods_list = List.map ( fun (s, t) ->
																						(s, unfold_type env t)
																			) list in
						TObject( name, methods_list)
		
		| TClass (name, list) ->
					let methods_list = List.map ( fun (s, t) ->
																						(s, unfold_type env t)
																			) list in
						TClass( name, methods_list)
  	
		| TUnit -> TUnit
  	
		| TNone error -> TNone ( error )
  	
		| TUndefined -> TUndefined;;

let rec string_of_iType t =
	match t with
  	| TNumber -> "Number"
  	| TString -> "String"
  	| TBoolean -> "Boolean"

  	| TFun (list, t) -> 
					let params_list = List.map (fun t -> string_of_iType t) list in
					let list_string = String.concat ", " params_list in
						"TFun( [" ^ list_string ^ "] ):" ^ (string_of_iType t)
		
  	| TProc list -> 
					let params_list = List.map (fun t -> string_of_iType t) list in
					let list_string = String.concat ", " params_list in
						"TProc( [" ^ list_string ^ "] )"
		
  	| TArray (size, t) -> 
					"TArray( " ^ (string_of_int size) ^ ", " ^ (string_of_iType t)^" ) "
  	
		| TRecord list -> 
					let fields_list = 
								List.map (fun (s, t) -> 
															" (" ^ s ^ "," ^ (string_of_iType t) ^")"
													) list in
					let list_string = String.concat ", " fields_list in
						"TRecord([ " ^ list_string ^ "] )"
  	
		| TRef r -> "TRef("^(string_of_iType !r)^")"
		
		| TClass_id id -> "TClass_id ( "^id^" ) "
		
		| TObject (name, list) ->
					let methods_list = 
								List.map (fun (s, t) ->
															"( " ^ s ^ ", " ^ (string_of_iType t) ^ " )"
													) list in
					let list_string = String.concat ", " methods_list in
						"TObject( " ^ name ^ ", [ " ^ list_string ^ " ]) "
		
		| TClass (name, list) ->
					let methods_list = 
								List.map (fun (s, t) ->
															"( " ^ s ^ ", " ^ (string_of_iType t) ^ " )"
													) list in
					let list_string = String.concat ", " methods_list in
						"TClass( " ^ name ^ ", [ " ^ list_string ^ " ]) "
  	
		| TUnit -> "Unit"
  	
		| TNone error -> "None ( " ^ error ^ " ) "
  	
		| TUndefined -> "Undefined";;

let rec contains t1 t2 compare_list =
	List.fold_left (fun prev_found (t1', t2') ->
											if t1' = t1 && t2' = t2 then
												true
											else
												prev_found
									) false compare_list



let rec equals env compare_list t1 t2 =
  (* if contains t1 t2 compare_list then                *)
  (* 	true                                             *)
  (* else                                               *)
  (* 	let new_compare_list = (t1, t2)::compare_list in *)
  (* 	let equals' = equals env new_compare_list in     *)
  	let equals' = equals env compare_list in
  	
  	(* print_string ("left: "^(string_of_iType t1)^"\nright: "^(string_of_iType t2)^"\n\n"); *)
  	match t1, t2 with
  		| TRef r1, _ -> equals' !r1 t2
  		| _, TRef r2 -> equals' t1 !r2
  		| TRecord list1, TRecord list2 ->
  					List.fold_left2 (fun prev_equals (s1, t1) (s2, t2) ->
  								prev_equals && s1 = s2 && (equals' t1 t2)
  													) true list1 list2
  
  		| TArray (size1, t1), TArray (size2, t2) ->
  					size1 = size2 && (equals' t1 t2)
  
  		| TFun (list1, t1), TFun (list2, t2) ->
  					let matching_args = List.fold_left2 (fun prev_match t1' t2' ->
  																									prev_match && (equals' t1' t2')
  																							) true list1 list2 in
  						matching_args && equals' t1 t2
  
  		| TProc list1, TProc list2 ->
  					List.fold_left2 (fun prev_match t1' t2' ->
  															(equals' t1' t2') && prev_match
  													) true list1 list2
  
  		| TClass_id name1, _ -> 
            let list1 = List.assoc name1 env in
            let new_t1 = TObject("", list1) in
              equals' new_t1 t2
  
  		| _, TClass_id name2 -> 
            let list2 = List.assoc name2 env in
            let new_t2 = TObject("", list2) in
              equals' t1 new_t2
  
  		| TNone _, TNone _ -> 
  					true
  
  		| TObject (name1, list1) , TObject (name2, list2) ->
            if contains t1 t2 compare_list then
              true
            else
              let new_compare_list = (t1, t2)::compare_list in
              let new_env1 = if name1 <> "" then (name1, list1)::env else env in
              let new_env2 = if name2 <> "" then (name2, list2)::new_env1 else new_env1 in
      					List.fold_left2 (fun prev_compare (s1, t1) (s2, t2) ->
													if prev_compare && s1 = s2 && equals new_env2 new_compare_list t1 t2 then
														true
													else 
														false
										) true list1 list2
  
  		| _ ->
  					t1 = t2

let rec subst name t t_method =
	match t_method with
		| TArray (length, t') -> TArray (length, (subst name t t'))
  	| TRecord list -> 
					let new_list = List.map (fun (s, t') ->
																				(s, subst name t t')
																	) list in
						TRecord new_list

  	| TRef r -> 
					TRef (ref (subst name t !r))

  	| TClass_id s -> 
					if s = name then
						t
					else
						TClass_id s

		| TFun (list, t') -> 
					let new_list = List.map (fun t'' ->
																				subst name t t''
																	) list in
					let new_t = subst name t t' in
							TFun(new_list, new_t)

		| TProc list -> 
					let new_list = List.map (fun t'' ->
																				subst name t t''
																	) list in
							TProc new_list

  	| TClass (s, list) ->
					if s <> name then (
						let new_list = List.map (fun (s, t') ->
																					(s, subst name t t')
																		) list in
							TClass (s, new_list)
					) else (
						TClass (s, list)
					)

  	| TObject (s, list) ->
					if s <> name then (
						let new_list = List.map (fun (s, t') ->
																					(s, subst name t t')
																		) list in
							TObject (s, new_list)
					) else (
						TObject (s, list)
					)
		| _ -> t_method

let get_type_of_array t =
	match t with
		| TArray(_, t) -> t
		| _ -> TNone "Array expected";;

let get_type_of_record env s t =
	match t with
		| TRecord list -> 
					(try
						let t' = List.assoc s list in
							(*unfold_type env*) t'
					with
						| Not_found -> TNone ("Attribute not found("^s^")"))

		| TObject (name, list) ->
    			(try
    				let t_method = List.assoc s list in
							subst name t t_method
    			with
    				| Not_found -> TNone ("Method not found("^s^")"))

		| _ -> TNone "Record expected";;


let bin_oper_int t1 t2 =
	match t1, t2 with
		| TNumber, TNumber -> true
		| _ -> false;;
	
let bin_oper_str t1 t2 =
	match t1, t2 with
		| TString, TString -> true
		| _ -> false;;

let bin_oper_bool t1 t2 =
	match t1, t2 with
		| TBoolean, TBoolean -> true
		| _ -> false;;
	
let un_oper_int t =
	match t with
		| TNumber -> true
		| _ -> false;;

let un_oper_str t =
	match t with
		| TString -> true
		| _ -> false;;
	
let un_oper_bool t =
	match t with
		| TBoolean -> true
		| _ -> false;;

let un_oper_array t =
	match t with
		| TArray _ -> true
		| _ -> false;;

let un_oper_record s t =
	match t with
		| TRecord list -> List.mem_assoc s list
		| TObject (name, list)-> List.mem_assoc s list
		| _ -> false;;

let not_none t =
	match t with
		| TNone _ -> false
		| _ -> true;;

let rec is_readable t =
	match t with
		| TRef r -> (match !r with
									| TNumber -> true
									| TString -> true
									| TBoolean -> true
									| _ -> false
								)
		| _ -> false;;

let rec is_writable t =
	match t with
		| TNumber -> true
		| TString -> true
		| TBoolean -> true
		| _ -> false;;

let rec get_reference_to env t =
	match t with
		| TArray (length, t') -> 
					TRef (ref (TArray(length, (get_reference_to env t'))))
		
		| TRecord list -> 
					let list' = 
							List.map (fun (s, t') ->
														(s, get_reference_to env t')
												) list in
					TRef (ref (TRecord list'))

		| TClass_id s -> 
					let t' = MyMap.find s env in
						get_reference_to env t'

		| _ -> TRef (ref t);;

