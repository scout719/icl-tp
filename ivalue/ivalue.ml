open Blaise_syntax
open Blaise_iType

module RecordMap = Map.Make (String)
module EnvMap = Map.Make (String)

type env = ivalue EnvMap.t

and ivalue =
	| StringValue of string
	| NumberValue of int
	| BooleanValue of bool
	| ArrayValue of ivalue array
	| RecordValue of ivalue RecordMap.t
	| RefValue of ivalue ref
	| FunValue of ((string * iType) list) * (decl_block list) * statement * iType * env ref
	| ProcValue of ((string * iType) list) * (decl_block list) * statement * env ref
	| NoneValue

let rec defaultValue t =
	match t with
	| TNumber -> NumberValue(0)
	| TString -> StringValue("")
	| TBoolean -> BooleanValue(false)
	| TArray (size, t1) -> 
			let array = Array.init size (fun i -> RefValue(ref (defaultValue t1))) in
				ArrayValue(array)
	| TRecord list -> 
			let record = List.fold_left (fun prev (s, t1) -> RecordMap.add s (RefValue(ref (defaultValue t1))) prev) RecordMap.empty list in
				RecordValue(record)
	| TObject (x, list) ->
			let new_list = List.fold_left( fun prev_list (s, _) ->
					prev_list @ [(s, TNone "dummy")]
																		) [] list in
				defaultValue (TRecord(new_list))
	| TClass_id _ -> RecordValue(RecordMap.empty)
	| _ -> NoneValue

let rec sum_ivalue e1 e2 =
	match e1, e2 with
	| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 + n2)
	| StringValue(s1), StringValue(s2) -> StringValue(s1^s2)
	| _ -> NoneValue

let rec sub_ivalue e1 e2 =
	match e1, e2 with
	| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 - n2)
	| _ -> NoneValue

let rec mult_ivalue e1 e2 =
	match e1, e2 with
	| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 * n2)
	| _ -> NoneValue

let rec div_ivalue e1 e2 =
	match e1, e2 with
	| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 / n2)
	| _ -> NoneValue

let rec mod_ivalue e1 e2 =
	match e1, e2 with
	| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 mod n2)
	| _ -> NoneValue

let rec eq_ivalue e1 e2 =
	match e1, e2 with
	| NumberValue(n1), NumberValue(n2) -> BooleanValue(n1 = n2)
	| StringValue(s1), StringValue(s2) -> BooleanValue(s1 = s2)
	| _ -> NoneValue

let rec gt_ivalue e1 e2 =
	match e1, e2 with
	| NumberValue(n1), NumberValue(n2) -> BooleanValue(n1 > n2)
	| StringValue(s1), StringValue(s2) -> BooleanValue(s1 > s2)
	| _ -> NoneValue

let rec not_ivalue e =
	match e with
	| BooleanValue(b) -> BooleanValue(not b)
	| _ -> NoneValue

let rec lt_ivalue e1 e2 =
	match e1, e2 with
	| NumberValue(n1), NumberValue(n2) -> BooleanValue(n1 < n2)
	| StringValue(s1), StringValue(s2) -> BooleanValue(s1 < s2)
	| _ -> NoneValue

let rec or_ivalue e1 e2 =
	match e1, e2 with
	| BooleanValue(b1), BooleanValue(b2) -> BooleanValue(b1 || b2)
	| _ -> NoneValue

let rec and_ivalue e1 e2 =
	match e1, e2 with
	| BooleanValue(b1), BooleanValue(b2) -> BooleanValue(b1 && b2)
	| _ -> NoneValue

let rec get_record_ivalue v s = 
	match v with
	| RecordValue(map) -> RecordMap.find s map
	| _ -> NoneValue

let rec get_array_ivalue array index =
	match array, index with
	| ArrayValue(array), NumberValue(n) -> Array.get array n
	| _ -> NoneValue

let rec string_of_ivalue e =
	match e with
	| NumberValue n -> string_of_int n
	| BooleanValue b -> string_of_bool b
	| StringValue s -> s
	| RefValue r -> string_of_ivalue !r

	| ArrayValue array -> 
			(Array.fold_left (fun prev v -> 
															prev^" "^(string_of_ivalue v)
												) "[ " array)^"]"
												
	| RecordValue record -> 
			"{ "^(RecordMap.fold (fun k v prev -> 
																			prev^k^":"^(string_of_ivalue v)^" "
														) record "")^"}"
														
	| ProcValue _ -> "Procedure's closure"
	| FunValue _ -> "Function's closure"
	| NoneValue -> "NoneValue"
