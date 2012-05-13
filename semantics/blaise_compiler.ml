open Blaise_syntax;;
open Blaise_typechk;;
open Blaise_iType;;

module StackframeMap = Map.Make (String);;

(** ********************************************     FALTA     ************************************************ *)

(* dereferenciacao implicita *)
(* compilar record e arrays *)
(* vericar duplicados no typecheck *)
(* Eq and Neq for strings*)

(** ********************************************   COMPILER    ************************************************ *)

exception Not_found;;

(** ********************************************     TYPES     *********************************************** *)

let int32 = "int32";;
let bool = "bool";;
let string = "string";;
let objecT = "object";;

(** ********************************************   OBJECT OPERS *********************************************** *)

let box_int32 = ["box int32"];;
let box_bool = ["box bool"];;
let unbox_int32 = "unbox int32" :: ["ldobj int32"];;
let unbox_bool = "unbox bool" :: ["ldobj bool"];;

(** ********************************************   LOADS       ************************************************ *)

let ldc n = ["ldc.i4 "^(string_of_int n)];;
let ldc_int32 n = (ldc n) @ (box_int32);;
let ldc_bool b = 
		if b then
			(ldc 1) @ box_bool
		else
			(ldc 0) @ box_bool;;
let ldstr s = ["ldstr \""^ s ^"\""];;
let ldloc_stackframe = ["ldloc stackframe" ];;

(** ********************************************   OPERATORS   ************************************************ *)

let add = "add" :: box_int32;;
let sub = "sub" :: box_int32;;
let mul = "mul" :: box_int32;;
let div = "div" :: box_int32;;
let rem = "rem" :: box_int32;;
let anD = "and" :: box_bool;;
let oR = "or" :: box_bool;;
let noT = "neg" :: "not" :: box_bool;;
let eq = "ceq" :: box_bool;;
let gt = "cgt" :: box_bool;;
let lt = "clt" :: box_bool;;
let brfalse label = ["brfalse "^ label];;
let br label = ["br "^ label];;
let swap = ["stloc tmp";
						"stloc tmp2";
						"ldloc tmp";
						"ldloc tmp2"];;
let dup = ["dup"];;
let nop label = [label^": nop"];;
let string_concat = ["call string [mscorlib]System.String::Concat(string, string)"];;

(** ******************************************** FUN/PRC OPERS ************************************************ *)

let ldftn_proc n = ["ldftn void f"^(string_of_int n)^"(object)"];;
let ldftn_fun n = ["ldftn object f"^(string_of_int n)^"(object)"];;
let call_fun = ["calli object(object)"];;
let call_proc = ["calli void(object)"];;

(** ******************************************** CLASS OPERS   ************************************************ *)

let print typE = ["call void class[mscorlib]System.Console::Write("^typE^")"];;
let new_cell =  ["newobj instance void [Runtime]Cell::.ctor(object)"];;
let cell_get =  ["callvirt instance object [Runtime]Cell::get()"];;
let cell_set =  ["callvirt instance void [Runtime]Cell::set(object)"];;
let new_stack =  ["newobj instance void [Runtime]StackFrame::.ctor(object)"];;
let stack_get = ["callvirt instance object [Runtime]StackFrame::get(int32)"];;
let stack_set = ["callvirt instance void [Runtime]StackFrame::set(int32, object)"];;
let stack_init_locals = ["callvirt instance void [Runtime]StackFrame::initLocals(int32)"];;
let stack_init_args = ["callvirt instance void [Runtime]StackFrame::initArgs(int32)"];;
let new_closure = ["newobj instance void [Runtime]Closure::.ctor(class [Runtime]StackFrame,native int)"];;
let closure_get_SF = ["callvirt instance class [Runtime]StackFrame [Runtime]Closure::getSF()"];;
let closure_get_Ftn = ["callvirt instance native int [Runtime]Closure::getFtn()"];;

(** ******************************************** PREAMBLES/FOOTERS ******************************************** *)

let preamble num_locals = [".assembly 'Blaise' {}";
    											 ".assembly extern Runtime {}";
    											 ".module Blaise.exe";
    											 ".method public static hidebysig default void Main () cil managed";
    											 "{";
    													".entrypoint";
    													".locals init (object stackframe, object tmp, object tmp2)";
    													"ldnull";
    													"newobj instance void [Runtime]StackFrame::.ctor(object)";
    													"stloc stackframe";
    													"ldloc stackframe";
    													"ldc.i4 "^(string_of_int num_locals);
    													"callvirt instance void [Runtime]StackFrame::initLocals(int32)"];;

let preamble_proc num_proc num_locals = [".method public static hidebysig default void f"^(string_of_int num_proc)^" (object) cil managed";
                  											 "{";
                  													".locals init (object stackframe, object tmp, object tmp2)";
                  													"ldarg 0";
                  													"stloc stackframe";
                  													"ldloc stackframe";
                  													"ldc.i4 "^(string_of_int num_locals);
                  													"callvirt instance void [Runtime]StackFrame::initLocals(int32)"];;

let preamble_fun num_fun num_locals = [".method public static hidebysig default object f"^(string_of_int num_fun)^" (object) cil managed";
                											 "{";
                													".locals init (object stackframe, object tmp, object tmp2)";
                													"ldarg 0";
                													"stloc stackframe";
                													"ldloc stackframe";
                													"ldc.i4 "^(string_of_int num_locals);
                													"callvirt instance void [Runtime]StackFrame::initLocals(int32)"];;
let footer = [	"ret";
							"}"];;

(** ******************************************** COMPILE OPERS ************************************************ *)

let compile_add l r t =
	match t with
		| TNumber -> l @ unbox_int32 @ r @ unbox_int32 @ add
		| TString -> l @ r @ string_concat
		| _ -> [] (* dummy *);;

let compile_bin_oper_int l r t oper =
	match t with
		| TNumber -> l @ unbox_int32 @ r @ unbox_int32 @ oper
		| _  -> [] (* dummy *);;

let compile_bin_oper_bool l r t oper =
	match t with
		| TBoolean -> l @ unbox_bool @ r @ unbox_bool @ oper
		| _  -> [] (* dummy *);;

let compile_un_oper_int e t oper =
	match t with
		| TNumber -> e @ unbox_int32 @ oper
		| _  -> [] (* dummy *);;

let compile_un_oper_bool e t oper =
	match t with
		| TBoolean -> e @ unbox_bool @ oper
		| _  -> [] (* dummy *);;

let write_type t =
	match t with
		| TNumber -> unbox_int32 @ (print int32)
		| TBoolean -> unbox_bool @ (print bool)
		| TString -> print string
		| TRecord _ -> print objecT
		| TArray _ -> print objecT

(** ******************************************** AUX FUNCTIONS ************************************************ *)

let identifier = ref 0;;

let fresh_identifier () =
	let curr = !identifier in
		identifier := curr + 1;
		curr;;
									
let label = ref 0;;

let next_label () = 
	let curr = !label in
		label := curr + 1;
		"L"^(string_of_int (curr));;

let locals = ref [0];;

let get_locals () =
	try
		List.nth !locals 0
	with
		| Failure _ -> 0;;

let inc_locals () =
	match !locals with
		| [] -> ()
		| x::xs ->
					locals := (x + 1) :: xs;;

let end_locals () =
	match !locals with
		| [] -> -1
		| x::xs -> 
					locals := xs; 
					x;;

let begin_locals () =
	locals := 0 :: (!locals);;

let rec find_idx_rec f l i =
	match l with
		| [] -> raise Not_found
		| x::xs -> if f x then
								(x, i)
							else find_idx_rec f xs (i+1);;

let find_idx f l = 
	find_idx_rec f l 0;;

let find s env =
	let f = fun (sfL, c) -> List.mem_assoc s sfL in
		let  sf, jumps = find_idx f env in
			let offset = List.assoc s (fst sf) in
				(jumps, offset);;

let assoc x env =
	(* if List.length env = 0 then ( 
		let new_stackframe = StackframeMap.add x 0 StackframeMap.empty in
			[(new_stackframe, ref 1)]
	)else *)
	let sf, count = List.hd env in
	let new_count = !count + 1 in
	let new_sf = StackframeMap.add x (new_count) sf in
		count := new_count;
		(new_sf, count) :: (List.tl env);;
			
let begin_scope env =
	(StackframeMap.empty, ref 0) :: env;;
	
let end_scope env =
	match env with
		| [] -> []
		| _::rest -> rest;;

let rec get_jumps_list n list =
	if n = 0 then
		list
	else
		get_jumps_list (n - 1) (list @ (ldc 0) @ (stack_get));;

let rec compile_expr env e =
	let compile_expr' = compile_expr env in
	match e with
		| Number n -> (ldc_int32 n, [])

		| Boolean b -> (ldc_bool b, [])

		| String s -> (ldstr s, [])

		| Add (l, r, t) -> 
					let comp_l, list_l = compile_expr' l in
					let comp_r, list_r = compile_expr' r in
						(compile_add comp_l comp_r t , list_l @ list_r)
		
		| Sub (l, r, t) -> 
					let comp_l, list_l = compile_expr' l in
					let comp_r, list_r = compile_expr' r in
						(compile_bin_oper_int comp_l comp_r t sub, list_l @ list_r)
						
		| Mult (l, r, t) -> 
					let comp_l, list_l = compile_expr' l in
					let comp_r, list_r = compile_expr' r in
						(compile_bin_oper_int comp_l comp_r t mul, list_l @ list_r)
						
		| Div (l, r, t) -> 
					let comp_l, list_l = compile_expr' l in
					let comp_r, list_r = compile_expr' r in
						(compile_bin_oper_int comp_l comp_r t div, list_l @ list_r)
						
		| Compl (e, t) -> 
					let comp_e, list_e = compile_expr' e in
					let comp_0, _ = compile_expr' (Number 0) in
						(compile_bin_oper_int comp_0 comp_e t sub, list_e)
						
		| Mod (l, r, t) -> 
					let (comp_l, list_l) = compile_expr' l in
					let (comp_r, list_r) = compile_expr' r in
						(compile_bin_oper_int comp_l comp_r t rem, list_l @ list_r)
						
		| And (l, r, t) -> 
					let comp_l, list_l = compile_expr' l in
					let comp_r, list_r = compile_expr' r in
						(compile_bin_oper_bool comp_l comp_r t anD, list_l @ list_r)
						
		| Or (l, r, t) -> 
					let comp_l, list_l = compile_expr' l in
					let comp_r, list_r = compile_expr' r in
						(compile_bin_oper_bool comp_l comp_r t oR, list_l @ list_r)
						
		| Not (e, t) -> 
					let comp_e, list_e = compile_expr' e in
						(compile_un_oper_bool comp_e t noT, list_e)
						
		| Eq (l, r, t) -> 
					let comp_l, list_l = compile_expr' l in
					let comp_r, list_r = compile_expr' r in
						(compile_bin_oper_bool comp_l comp_r t eq, list_l @ list_r)
						
		| Gt (l, r, t) -> 
					let comp_l, list_l = compile_expr' l in
					let comp_r, list_r = compile_expr' r in
						(compile_bin_oper_bool comp_l comp_r t gt, list_l @ list_r)
						
		| Lt (l, r, t) -> 
					let comp_l, list_l = compile_expr' l in
					let comp_r, list_r = compile_expr' r in
						(compile_bin_oper_bool comp_l comp_r t lt, list_l @ list_r)
						
		| Gteq (l, r, t) -> 
					let comp_l, list_l = compile_expr' l in
					let comp_r, list_r = compile_expr' r in
					let comp_gt = compile_bin_oper_bool comp_l comp_r t gt in
					let comp_eq = compile_bin_oper_bool comp_l comp_r t eq in
						(compile_bin_oper_bool comp_gt comp_eq t oR, list_l @ list_r)
						
		| Lteq (l, r, t) -> 
					let comp_l, list_l = compile_expr' l in
					let comp_r, list_r = compile_expr' r in
					let comp_lt = compile_bin_oper_bool comp_l comp_r t lt in
					let comp_eq = compile_bin_oper_bool comp_l comp_r t eq in
						(compile_bin_oper_bool comp_lt comp_eq t oR, list_l @ list_r)
						
		| Neq (l, r, t) -> 
					let comp_l, list_l = compile_expr' l in
					let comp_r, list_r = compile_expr' r in
					let comp_eq = compile_bin_oper_bool comp_l comp_r t eq in
						(compile_un_oper_bool comp_eq t noT, list_l @ list_r)

		| Id (s, _)  -> 
					let jumps, offset = find s env in
					let jump_comp = get_jumps_list jumps [] in
						(ldloc_stackframe @ jump_comp @ (ldc offset) @ stack_get, [])

		| CallFun(e, list, _) -> 
				let comp_e, list_e = compile_expr' e in
				let (args_comp, args_list, last_index) = 
					List.fold_left( fun (prev_args, prev_list, prev_index) e ->
															let (comp_e, list_e) = compile_expr' e in
															let new_args_comp = 
																dup @ 
																(ldc prev_index) @ 
																comp_e @ 
																stack_set @ 
																prev_args in
															let new_list = prev_list @ list_e in
															let new_index = prev_index + 1 in
																(new_args_comp, new_list, new_index)
																							) ([],[], 1) list in
				let comp_fun = 
					comp_e @ 
					dup @ 
					closure_get_SF @ 
					new_stack @ 
					dup @ 
					(ldc (last_index - 1)) @ 
					stack_init_args @ 
					args_comp @ 
					swap @ 
					closure_get_Ftn @ 
					call_fun in
						(comp_fun, list_e @ args_list)
														
(*		| Var (e1, _) -> let (s , f) = code_genAux' e1 in
											(s@newObject, f)

		| Deref (e1, _) -> let (s , f) = code_genAux' e1 in
											(s@cellGet, f)

		| Id (s, _)  -> let (jumps, offset) = find s env in
											let jump_comp = getJumps jumps in
												((ldloc stackframe)@(jump_comp)@(ldc offset)@stackGet, [])
												
		| Do (s, e, _) -> let (s1, f1) = code_genStateAux env s in
												let (s2, f2) = code_genAux' e in
													(s1@s2, f1@f2)
													
		| DeclE(list, e, _) ->
											let new_env = List.fold_left (fun prev (x,_) -> incLocals(); assoc x prev) env list in
													let (declarations, opers) = List.fold_left (
																fun (prev_decl, prev_f) (x,y) -> let (_, addr) = find x new_env in
																																		let (s1, f1) = code_genAux' y in
																																			(prev_decl@(ldloc stackframe)@(ldc addr)@s1@stackSet, prev_f@f1)
																														) ([], []) list in
															let (s2, f2) = code_genAux new_env e in
																(declarations@s2, opers@f2)
		| Proc(list, s, _) -> 
				let n = freshIdentifier () in
					let tmp_env = beginScope env in
						beginLocals();
						let new_env = List.fold_left (fun prev_env s ->	
																						assoc s prev_env
																				) tmp_env list in
							let (sE, f) = code_genStateAux new_env s in
							let numLocals = endLocals() in
							let declProc = (preambleProc numLocals)@sE@footerProc in
								((ldloc stackframe)@(ldftnProc n)@newClosure, declProc@f)
								
		| Fun(list, e, _) -> 
				let n = freshIdentifier () in
					let tmp_env = beginScope env in
						beginLocals();
						let new_env = List.fold_left (fun prev_env s ->	
																						assoc s prev_env
																				) tmp_env list in
							let (sE, f) = code_genAux new_env e in
							let numLocals = endLocals() in
							let declProc = (preambleFun numLocals)@sE@footerFun in
								((ldloc stackframe)@(ldftnFun n)@newClosure, declProc@f)
								
		| CallF(e, list, _) -> 
				let (s1, f1) = code_genAux' e in
				let (declList, fList, length) = List.fold_left( fun (prevDecl, prevF, prevIndex) e	-> 
																												let (c1, f) = code_genAux' e in
																													(dup @ (ldc prevIndex)@ c1 @ stackSet @ prevDecl,prevF @ f, prevIndex + 1)
																							) ([],[], 1) list in
				let final = s1 @ dup @ closureGetSF @ newStack @ dup @ (ldc (length - 1)) @ stackInitArgs @ declList @ swap @ closureGetFTN @ callFun in
					(final, f1@fList)
				
				
		*)
									

and compile_stat env s =
	let compile_expr' = compile_expr env in
	let compile_stat' = compile_stat env in	
	match s with
		| Seq(l, r, _) ->
					let comp_l, list_l = compile_stat' l in
					let comp_r, list_r = compile_stat' r in
						(comp_l @ comp_r, list_l @ list_r)
						
		| If(e, s, _) -> 
					let label = next_label () in
					let comp_e, list_e = compile_expr' e in
					let comp_s, list_s = compile_stat' s in
						(comp_e @ unbox_bool @ (brfalse label) 
										@ comp_s @ (nop label), list_e @ list_s)
					
		| If_Else(e, s1, s2, _) -> 
					let label1 = next_label () in
					let label2 = next_label () in
					let comp_e, list_e = compile_expr' e in
					let comp_s1, list_s1 = compile_stat' s1 in
					let comp_s2, list_s2 = compile_stat' s2 in
						(comp_e @ unbox_bool @ (brfalse label1) 
										@ comp_s1 @ (br label2) 
						@ (nop label1) 
										@ comp_s2 @ (nop label2), list_e @ list_s1 @ list_s2)

		| While(e, s, _) -> 
					let label1 = next_label () in
					let label2 = next_label () in
					let comp_e, list_e = compile_expr' e in
					let comp_s, list_s = compile_stat' s in
						((nop label1) @ comp_e @ unbox_bool @ (brfalse label2)
													@ comp_s @ (br label1) @ (nop label2), 
																															list_e @ list_s)
		
		| Write (list, _) -> 
					List.fold_left (fun (prev_comp, prev_list) e -> 
														let t = get_type e in
														let (comp_e, list_e) = compile_expr' e in
														let comp_print = comp_e @ (write_type t) in
															(prev_comp @ comp_print, prev_list @ list_e)) ([], []) list
	(*												
		| If(e1, s1, s2, _) -> let label1 = "L"^(string_of_int (nextLabel())) in
														let label2 = "L"^(string_of_int (nextLabel())) in
														let (s1c, f1) = code_genAux' e1 in
														let (s2c, f2) = code_genStateAux' s1 in
														let (s3c, f3) = code_genStateAux' s2 in
															(s1c@unboxBool@(brfalse label1)@s2c@(br label2)@[label1^": nop"]@s3c@[label2^": nop"], f1@f2@f3)

		| While(e1, s1, _) -> let label1 = "L"^(string_of_int (nextLabel())) in
													let label2 = "L"^(string_of_int (nextLabel())) in
													let (s1c, f1) = code_genAux' e1 in
													let (s2c, f2) = code_genStateAux' s1 in
														([label1^": nop"]@s1c@unboxBool@(brfalse label2)@s2c@(br label1)@[label2^": nop"], f1@f2)
														
		| Assign(e1, e2, _) -> let (se1, f1) = code_genAux' e1 in
														let (se2, f2) = code_genAux' e2 in
															(se1@se2@cellSet, f1@f2)
															
		| Print (e1, _) -> let (s, f) = code_genAux' e1 in
													(s@(print "object"), f)
		| Println -> (("ldstr \"\n\"")::(print "class System.String"), [])

		| DeclS(list, e, _) ->
											let new_env = List.fold_left (fun prev (x,_) -> incLocals(); assoc x prev) env list in
													let (declarations, opers) = List.fold_left (
																fun (prev_decl, prev_f) (x,y) -> let (_, addr) = find x new_env in
																																		let (s1, f1) = code_genAux' y in
																																			(prev_decl@(ldloc stackframe)@(ldc addr)@s1@stackSet, prev_f@f1)
																														) ([], []) list in
															let (s2, f2) = code_genStateAux new_env e in
																(declarations@s2, opers@f2)
		
		| CallP(e, list, _) -> 
				let (s1, f1) = code_genAux' e in
				let (declList, fList, length) = List.fold_left( fun (prevDecl, prevF, prevIndex) e	-> 
																												let (c1, f) = code_genAux' e in
																													(dup @ (ldc prevIndex)@ c1 @ stackSet @ prevDecl,prevF @ f, prevIndex + 1)
																							) ([],[], 1) list in
				let final = s1 @ dup @ closureGetSF @ newStack @ dup @ (ldc (length - 1)) @ stackInitArgs @ declList @ swap @ closureGetFTN @ callProc in
					(final, f1@fList)
													
					
let code_gen e =
	(*let ast = checkState [] e in
	if (getTypeStat ast) <> TNone then(
		resetAddr();*)
		let (program, opers) = code_genStateAux [] e in
				(preamble (getLocals() + 1))@program@footer@opers
	(* ) else
		raise TypeCheckFailed*)
		
		*)
		
let compile_program p =
	print_string "Hello World! xP";;
