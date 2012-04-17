type ivalue =
	| StringValue of string
	| NumberValue of int
	| BooleanValue of bool
	| RefValue of ivalue ref
	| None

let sum_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 + n2)
		| StringValue(s1), StringValue(s2) -> StringValue(s1^s2)
		| _ -> None

let sub_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 - n2)
		| _ -> None

let mult_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 * n2)
		| _ -> None

let div_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 / n2)
		| _ -> None

let mod_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> NumberValue(n1 mod n2)
		| _ -> None

let eq_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> BooleanValue(n1 = n2)
		| StringValue(s1), StringValue(s2) -> BooleanValue(s1 = s2)
		| _ -> None

let gt_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> BooleanValue(n1 > n2)
		| _ -> None

let not_ivalue e =
	match e with
		| BooleanValue(b) -> BooleanValue(not b)
		| _ -> None

let lt_ivalue e1 e2 =
	match e1, e2 with
		| NumberValue(n1), NumberValue(n2) -> BooleanValue(n1 < n2)
		| _ -> None

let or_ivalue e1 e2 =
	match e1, e2 with
		| BooleanValue(b1), BooleanValue(b2) -> BooleanValue(b1 || b2)
		| _ -> None

let and_ivalue e1 e2 =
	match e1, e2 with
		| BooleanValue(b1), BooleanValue(b2) -> BooleanValue(b1 && b2)
		| _ -> None

let string_of_ivalue e =
	match e with
		| NumberValue n -> string_of_int n
		| BooleanValue b -> string_of_bool b
		| StringValue s -> s
		| None -> "None"