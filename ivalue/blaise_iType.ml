type iType =
	| TNumber
	| TString
	| TBoolean
	| TFun of (iType list) * iType
	| TProc of (iType list)
	| TArray of int * iType
	| TRecord of (string * iType) list
	| TRef of iType ref
	| TClass of string * (string * (iType list) * iType) list
	| TObject of string * (string * (iType list) * iType) list
	| TClass_id of string
	| TUnit
	| TNone
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
					let list_string = List.fold_left (fun prev_list t ->
																								prev_list ^ (string_of_iType t) ^ " "
																						) "" list in
						"TFun( " ^ list_string ^ "):" ^ (string_of_iType t)
  	| TProc _ -> "TProv"
  	| TArray (_, t) -> "TArray("^(string_of_iType t)^")"
  	| TRecord list -> 
					let list_string = List.fold_left ( fun 	prev_list (s, t) ->
																									prev_list ^ " (" ^ s ^ "," ^ (string_of_iType t) ^")"
																						) "" list in
						"TRecord(" ^ list_string ^ " )"
  	| TRef r -> "TRef("^(string_of_iType !r)^")"
  	| TUnit -> "Unit"
  	| TNone -> "None"
  	| TUndefined -> "Undefined";;

let get_type_of_array t =
	match t with
		| TArray(_, t) -> t
		| _ -> TNone;;

let get_type_of_record s t =
	match t with
		| TRecord list -> 
					(try
						List.assoc s list
					with
						| Not_found -> TNone)
		| _ -> TNone;;

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
		| _ -> false;;

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
		
		| TRef r -> get_reference_to !r

		| _ -> TNone;; (* dummy *)