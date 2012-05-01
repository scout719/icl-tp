open Blaise_syntax

module RecordMap = Map.Make (String)

type env = (string * ivalue) list

and ivalue =
	| StringValue of string
	| NumberValue of int
	| BooleanValue of bool
	| ArrayValue of ivalue array
	| RecordValue of ivalue RecordMap.t
	| RefValue of ivalue ref
	| FunValue of ((string * iType) list) * (decl_block list) * statement * iType * env
	| ProcValue of ((string * iType) list) * (decl_block list) * statement * env
	| None

let rec defaultValue t =
	match t with
	| TNumber -> NumberValue(0)
	| TString -> StringValue("")
	| TBoolean -> BooleanValue(false)
	| TArray (size, t1) -> ArrayValue( Array.init size (fun i -> RefValue(ref (defaultValue t1))))
	| TRecord (list) -> let map = List.fold_left (fun prev (s, t1) -> RecordMap.add s (RefValue(ref (defaultValue t1))) prev) RecordMap.empty list in
												RecordValue(map)

let rec sum_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 + n2)
		| StringValue(s1), StringValue(s2) -> StringValue(s1^s2)
		| _ -> None

let rec sub_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 - n2)
		| _ -> None

let rec mult_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 * n2)
		| _ -> None

let rec div_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 / n2)
		| _ -> None

let rec mod_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 mod n2)
		| _ -> None

let rec eq_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> BooleanValue(n1 = n2)
		| StringValue(s1), StringValue(s2) -> BooleanValue(s1 = s2)
		| _ -> None

let rec gt_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> BooleanValue(n1 > n2)
		| _ -> None

let rec not_ivalue e =
	match e with
		| BooleanValue(b) -> BooleanValue(not b)
		| _ -> None

let rec lt_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> BooleanValue(n1 < n2)
		| _ -> None

let rec or_ivalue e1 e2 =
	match e1, e2 with
		| BooleanValue(b1), BooleanValue(b2) -> BooleanValue(b1 || b2)
		| _ -> None

let rec and_ivalue e1 e2 =
	match e1, e2 with
		| BooleanValue(b1), BooleanValue(b2) -> BooleanValue(b1 && b2)
		| _ -> None

let rec get_record_ivalue v s = 
	match v with
		| RecordValue(map) -> RecordMap.find s map
		| _ -> None

let rec get_array_ivalue array index =
	match array, index with
		| ArrayValue(array), NumberValue(n) -> Array.get array n
		| _ -> None

let rec string_of_ivalue e =
	match e with
		| NumberValue n -> string_of_int n
		| BooleanValue b -> string_of_bool b
		| StringValue s -> s
		| RefValue r -> string_of_ivalue !r
		| None -> "None"