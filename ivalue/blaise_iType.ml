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

let rec string_of_iType t =
	match t with
  	| TNumber -> "Number"
  	| TString -> "String"
  	| TBoolean -> "Boolean"

  	| TFun (list, t) -> 
					let params_list = List.fold_left (fun prev_list t ->
																								prev_list @ [string_of_iType t]
																						) [] list in
					let list_string = String.concat ", " params_list in
						"TFun( [" ^ list_string ^ "] ):" ^ (string_of_iType t)
		
  	| TProc list -> 
					let params_list = List.fold_left (fun prev_list t ->
																								prev_list @ [string_of_iType t]
																						) [] list in
					let list_string = String.concat ", " params_list in
						"TProc( [" ^ list_string ^ "] )"
		
  	| TArray (size, t) -> 
					"TArray( " ^ (string_of_int size) ^ ", " ^ (string_of_iType t)^" ) "
  	
		| TRecord list -> 
					let fields_list = List.fold_left ( fun 	prev_list (s, t) ->
																									prev_list @ [" (" ^ s ^ "," ^ (string_of_iType t) ^")"]
																						) [] list in
					let list_string = String.concat ", " fields_list in
						"TRecord([ " ^ list_string ^ "] )"
  	
		| TRef r -> "TRef("^(string_of_iType !r)^")"
		
		| TClass_id id -> "TClass_id ( "^id^" ) "
		
		| TObject (name, list) ->
					let methods_list = List.fold_left ( fun 	prev_list (s, t) ->
																									prev_list @ ["( " ^ s ^ ", " ^ (string_of_iType t) ^ " )"]
																						) [] list in
					let list_string = String.concat ", " methods_list in
						"TObject( " ^ name ^ ", [ " ^ list_string ^ " ]) "
		
		| TClass (name, list) ->
					let methods_list = List.fold_left ( fun 	prev_list (s, t) ->
																									prev_list @ ["( " ^ s ^ ", " ^ (string_of_iType t) ^ " )"]
																						) [] list in
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
	let equals' = equals env compare_list in
	
	(* print_string ("left: "^(string_of_iType t1)^"\nright: "^(string_of_iType t2)^"\n\n"); *)
	
	match t1, t2 with
		| TRecord list1, TRecord list2 ->
					List.fold_left2 (fun prev_equals (s1, t1) (s2, t2) ->
								prev_equals && s1 = s2 && (equals' t1 t2)
													) true list1 list2

		| TArray (size1, t1), TArray (size2, t2) ->
					size1 = size2 && (equals' t1 t2)

		| TRef r1, TRef r2 ->
					equals' !r1 !r2

		| TFun (list1, t1), TFun (list2, t2) ->
					let matching_args = List.fold_left2 (fun prev_match t1' t2' ->
																									prev_match && (equals' t1' t2')
																							) true list1 list2 in
						matching_args && equals' t1 t2

		| TProc list1, TProc list2 ->
					List.fold_left2 (fun prev_match t1' t2' ->
															(equals' t1' t2') && prev_match
													) true list1 list2

		| TClass_id name1, TClass_id name2 -> 
					let list1 = List.assoc name1 env in
					let new_t1 = TObject("", list1) in
					let list2 = List.assoc name2 env in
					let new_t2 = TObject("", list2) in
						equals' new_t1 new_t2

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

		| TObject _ , TClass_id name2 ->
					let list2 = List.assoc name2 env in
					let new_t2 = TObject("", list2) in
						equals' t1 new_t2

		| TClass_id name1, TObject _ ->
					let list1 = List.assoc name1 env in
					let new_t1 = TObject("", list1) in
						equals' new_t1 t2

		| _ ->
					t1 = t2

let rec subst name t t_method =
	match t_method with
		| TArray (length, t') -> TArray (length, (subst name t t'))
  	| TRecord list -> 
					let new_list = List.fold_left (fun prev_list (s, t') ->
																							prev_list @ [(s, subst name t t')]
																				) [] list in
						TRecord new_list

  	| TRef r -> 
					TRef (ref (subst name t !r))

  	| TClass_id s -> 
					if s = name then
						t
					else
						TClass_id s

		| TFun (list, t') -> 
					let new_list = List.fold_left (fun prev_list t'' ->
																							prev_list @ [subst name t t'']
																				) [] list in
					let new_t = subst name t t' in
							TFun(new_list, new_t)

		| TProc list -> 
					let new_list = List.fold_left (fun prev_list t'' ->
																							prev_list @ [subst name t t'']
																				) [] list in
							TProc new_list

  	| TClass (s, list) ->
					if s <> name then (
						let new_list = List.fold_left (fun prev_list (s, t') ->
																							prev_list @ [(s, subst name t t')]
																					) [] list in
							TClass (s, new_list)
					) else (
						TClass (s, list)
					)

  	| TObject (s, list) ->
					if s <> name then (
						let new_list = List.fold_left (fun prev_list (s, t') ->
																							prev_list @ [(s, subst name t t')]
																					) [] list in
							TObject (s, new_list)
					) else (
						TObject (s, list)
					)
		| _ -> t_method

let get_type_of_array t =
	match t with
		| TArray(_, t) -> t
		| _ -> TNone "Array expected";;

let get_type_of_record s t =
	match t with
		| TRecord list -> 
					(try
						List.assoc s list
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

let rec get_reference_to t =
	match t with
		| TNumber -> TRef(ref TNumber)
		
		| TString -> TRef(ref TString)
		
		| TBoolean -> TRef(ref TBoolean)
		
		| TArray (length, t') -> 
					TRef (ref (TArray(length, (get_reference_to t'))))
		
		| TRecord list -> 
					let list' = 
							List.fold_left (fun prev_list (s, t') ->
																	prev_list @ [(s, get_reference_to t')]
															) [] list in
					TRef (ref (TRecord list'))
		
		| TFun (list, t) -> 
					TRef (ref (TFun(list, t)))

		| TProc list -> 
					TRef (ref (TProc list))
		
		| TRef r -> TRef (ref t)

		| TClass _ -> TRef (ref t)

		| TObject _ -> TRef (ref t)

		| _ -> TNone "Internal error";; (* dummy *)

