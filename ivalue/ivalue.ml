open Blaise_syntax

type ivalue =
	| StringValue of string
	| NumberValue of int
	| BooleanValue of bool
	| RefValue of ivalue ref
	| FunValue of (string list) * statement * ((string * ivalue) list)
	| ProcValue of (string list) * statement * ((string * ivalue) list)
	| Uninitialized
	| None

let rec sum_ivalue e1 e2 =
	match e1, e2 with
		| RefValue(r), _ -> sum_ivalue !r e2
		| _, RefValue(r) -> sum_ivalue e1 !r
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 + n2)
		| StringValue(s1), StringValue(s2) -> StringValue(s1^s2)
		| _ -> None

let rec sub_ivalue e1 e2 =
	match e1, e2 with
		| RefValue(r), _ -> sub_ivalue !r e2
		| _, RefValue(r) -> sub_ivalue e1 !r
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 - n2)
		| _ -> None

let rec mult_ivalue e1 e2 =
	match e1, e2 with
		| RefValue(r), _ -> mult_ivalue !r e2
		| _, RefValue(r) -> mult_ivalue e1 !r
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 * n2)
		| _ -> None

let rec div_ivalue e1 e2 =
	match e1, e2 with
		| RefValue(r), _ -> div_ivalue !r e2
		| _, RefValue(r) -> div_ivalue e1 !r
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 / n2)
		| _ -> None

let rec mod_ivalue e1 e2 =
	match e1, e2 with
		| RefValue(r), _ -> mod_ivalue !r e2
		| _, RefValue(r) -> mod_ivalue e1 !r
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 mod n2)
		| _ -> None

let rec eq_ivalue e1 e2 =
	match e1, e2 with
		| RefValue(r), _ -> eq_ivalue !r e2
		| _, RefValue(r) -> eq_ivalue e1 !r
		| NumberValue(n1), NumberValue(n2) -> BooleanValue(n1 = n2)
		| StringValue(s1), StringValue(s2) -> BooleanValue(s1 = s2)
		| _ -> None

let rec gt_ivalue e1 e2 =
	match e1, e2 with
		| RefValue(r), _ -> gt_ivalue !r e2
		| _, RefValue(r) -> gt_ivalue e1 !r
		| NumberValue(n1), NumberValue(n2) -> BooleanValue(n1 > n2)
		| _ -> None

let rec not_ivalue e =
	match e with
		| RefValue(r) -> not_ivalue !r
		| BooleanValue(b) -> BooleanValue(not b)
		| _ -> None

let rec lt_ivalue e1 e2 =
	match e1, e2 with
		| RefValue(r), _ -> lt_ivalue !r e2
		| _, RefValue(r) -> lt_ivalue e1 !r
		| NumberValue(n1), NumberValue(n2) -> BooleanValue(n1 < n2)
		| _ -> None

let rec or_ivalue e1 e2 =
	match e1, e2 with
		| RefValue(r), _ -> or_ivalue !r e2
		| _, RefValue(r) -> or_ivalue e1 !r
		| BooleanValue(b1), BooleanValue(b2) -> BooleanValue(b1 || b2)
		| _ -> None

let rec and_ivalue e1 e2 =
	match e1, e2 with
		| RefValue(r), _ -> and_ivalue !r e2
		| _, RefValue(r) -> and_ivalue e1 !r
		| BooleanValue(b1), BooleanValue(b2) -> BooleanValue(b1 && b2)
		| _ -> None

let rec string_of_ivalue e =
	match e with
		| NumberValue n -> string_of_int n
		| BooleanValue b -> string_of_bool b
		| StringValue s -> s
		| RefValue r -> string_of_ivalue !r
		| Uninitialized -> "Uninitialized" 
		| None -> "None"