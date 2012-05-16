open Blaise_syntax;;
open Blaise_typechk;;
open Blaise_iType;;

module StackframeMap = Map.Make (String);;

(** ********************************************     FALTA     ************************************************ *)

(* assign and create arrays *)

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
let string_concat = ["callvirt instance string [mscorlib]System.String::Concat(string, string)"];;
let obj_equals = "callvirt instance bool [mscorlib]System.Object::Equals(object)" :: box_bool;;

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
let new_record = ["newobj instance void [Record]Record::.ctor()"];;
let record_get = ["callvirt instance object [Record]Record::GetValue(string)"];;
let record_set = ["callvirt instance void [Record]Record::SetValue(string, object)"];;
let new_array = ["newobj instance void [Array]Array::.ctor(int32, object)"];;
let array_get = ["callvirt instance object [Array]Array::Get(int32)"];;
let array_set = ["callvirt instance void [Array]Array::Set(int32, object)"];;

(** ******************************************** PREAMBLES/FOOTERS ******************************************** *)

let preamble num_locals = [".assembly 'Blaise' {}";
    											 ".assembly extern Runtime {}";
													 ".assembly extern Record {}";
													 ".assembly extern Array {}";
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

let compile_bin_oper_obj l r oper =
	l @ r @ oper;;

let compile_bin_oper_int l r oper =
	l @ unbox_int32 @ r @ unbox_int32 @ oper;;

let compile_bin_oper_bool l r oper =
	l @ unbox_bool @ r @ unbox_bool @ oper;;

let compile_un_oper_int e oper =
	e @ unbox_int32 @ oper;;

let compile_un_oper_bool e oper =
	e @ unbox_bool @ oper;;

let write_type t =
	match t with
		| TNumber -> unbox_int32 @ (print int32)
		| TBoolean -> unbox_bool @ (print bool)
		| TString -> print string
		| TRecord _ -> print objecT
		| TArray _ -> print objecT

let rec compile_default_type t =
	match t with
		| TNumber -> (ldc_int32 0) @ new_cell
		| TBoolean -> (ldc_bool false) @ new_cell
		| TString ->  (ldstr "") @ new_cell
		| TRecord list -> 
					let comp_set_record = 
							List.fold_left (fun prev_comp (s, t) ->
								let comp_t = compile_default_type t in
								let temp_comp = dup @ (ldstr s) @ comp_t @ record_set in
									prev_comp @ temp_comp 
														) [] list in
						new_record @ comp_set_record @ new_cell

		| TArray (length , t) -> 
					let comp_t = compile_default_type t in
						(ldc length) @ comp_t @ new_array @ new_cell

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

let locals = ref [];;

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

let rec find_idx_rec f env i =
	match env with
		| [] -> raise Not_found
		| x::xs -> if f x then
								(x, i)
							else find_idx_rec f xs (i+1);;

let find_idx f env = 
	find_idx_rec f env 0;;

let find s env =
	let f = fun (stackframe, _) -> StackframeMap.mem s stackframe in
	let stack, jumps = find_idx f env in
	let (offset, is_var) = StackframeMap.find s (fst stack) in
		(jumps, offset, is_var);;

let assoc x env is_var =
	let sf, count = List.hd env in
	let new_count = !count + 1 in
	let new_sf = StackframeMap.add x (new_count, is_var) sf in
		count := new_count;
		(new_sf, count) :: (List.tl env);;
			
let begin_scope env =
	(StackframeMap.empty, ref 0) :: env;;

let rec get_jumps_list n list =
	if n = 0 then
		list
	else
		get_jumps_list (n - 1) (list @ (ldc 0) @ (stack_get));;

(** ******************************************** EXPR COMPILER ************************************************ *)

let rec compile_expr env to_result e =
	let compile_expr' = compile_expr env true in
	match e with
		| Number n -> ldc_int32 n

		| Boolean b -> ldc_bool b

		| String s -> ldstr s

		| Array (list, _) ->
					let comp_set_array, length = 
							List.fold_left (fun (prev_comp, prev_index) e ->
									let comp_e = compile_expr' e in
									let temp_comp = dup @ (ldc prev_index) @ comp_e @ array_set in
									let new_index = prev_index + 1 in
										(prev_comp @ temp_comp, new_index)
															) ([], 0) list in
						(ldc length) @ new_array @ comp_set_array

		| Record (list, _) -> 
					let comp_set_record = 
							List.fold_left (fun prev_comp (s, e) ->
								let comp_e = compile_expr' e in
								let temp_comp = dup @ (ldstr s) @ comp_e @ record_set in
									prev_comp @ temp_comp 
														) [] list in
						new_record @ comp_set_record

		| Add (l, r, t) -> 
					let comp_l = compile_expr' l in
					let comp_r = compile_expr' r in
					let (operation, oper_code) = 
								if t = TNumber then
									(compile_bin_oper_int, add)
								else
									(compile_bin_oper_obj, string_concat) in
						operation comp_l comp_r oper_code
		
		| Sub (l, r, _) -> 
					let comp_l = compile_expr' l in
					let comp_r = compile_expr' r in
						compile_bin_oper_int comp_l comp_r sub
						
		| Mult (l, r, _) -> 
					let comp_l = compile_expr' l in
					let comp_r = compile_expr' r in
						compile_bin_oper_int comp_l comp_r mul
						
		| Div (l, r, _) -> 
					let comp_l = compile_expr' l in
					let comp_r = compile_expr' r in
						compile_bin_oper_int comp_l comp_r div
						
		| Compl (e, _) -> 
					let comp_e = compile_expr' e in
					let comp_0 = compile_expr' (Number 0) in
						compile_bin_oper_int comp_0 comp_e sub
						
		| Mod (l, r, _) -> 
					let comp_l = compile_expr' l in
					let comp_r = compile_expr' r in
						compile_bin_oper_int comp_l comp_r rem
						
		| And (l, r, _) -> 
					let comp_l = compile_expr' l in
					let comp_r = compile_expr' r in
						compile_bin_oper_bool comp_l comp_r anD
						
		| Or (l, r, _) -> 
					let comp_l = compile_expr' l in
					let comp_r = compile_expr' r in
						compile_bin_oper_bool comp_l comp_r oR
						
		| Not (e, _) -> 
					let comp_e = compile_expr' e in
						compile_un_oper_bool comp_e noT
						
		| Eq (l, r, _) -> 
					let comp_l = compile_expr' l in
					let comp_r = compile_expr' r in
						compile_bin_oper_obj comp_l comp_r obj_equals

		| Gt (l, r, _) -> 
					let comp_l = compile_expr' l in
					let comp_r = compile_expr' r in
					let type_l = unref_iType (get_type l) in
					let operation = 
								if type_l = TBoolean then
									compile_bin_oper_bool
								else
									compile_bin_oper_int in
						operation comp_l comp_r gt

		| Lt (l, r, _) -> 
					let comp_l = compile_expr' l in
					let comp_r = compile_expr' r in
					let type_l = unref_iType (get_type l) in
					let operation = 
								if type_l = TBoolean then
									compile_bin_oper_bool
								else
									compile_bin_oper_int in
						operation comp_l comp_r lt

		| Gteq (l, r, _) -> 
					let comp_l = compile_expr' l in
					let comp_r = compile_expr' r in
					let type_l = unref_iType (get_type l) in
					let operation = 
								if type_l = TBoolean then
									compile_bin_oper_bool
								else
									compile_bin_oper_int in
					let comp_gt = operation comp_l comp_r gt in
					let comp_eq = operation comp_l comp_r eq in
						compile_bin_oper_bool comp_gt comp_eq oR

		| Lteq (l, r, _) -> 
					let comp_l = compile_expr' l in
					let comp_r = compile_expr' r in
					let type_l = unref_iType (get_type l) in
					let operation = 
								if type_l = TBoolean then
									compile_bin_oper_bool
								else
									compile_bin_oper_int in
					let comp_lt = operation comp_l comp_r lt in
					let comp_eq = operation comp_l comp_r eq in
						compile_bin_oper_bool comp_lt comp_eq oR

		| Neq (l, r, _) -> 
					let comp_l = compile_expr' l in
					let comp_r = compile_expr' r in
					let type_l = unref_iType (get_type l) in
					let operation_eq = 
        				if type_l = TBoolean then
									compile_bin_oper_bool
								else
									compile_bin_oper_int in
					let comp_eq = operation_eq comp_l comp_r eq in
						compile_un_oper_bool comp_eq noT

		| Id (s, t)  -> 
					let jumps, offset, _ = find s env in
					let jump_comp = get_jumps_list jumps [] in
					let var = is_var t in
					let deref = 
							if var && to_result then
								cell_get
							else
								[] in
						ldloc_stackframe @ jump_comp @ (ldc offset) @ stack_get @ deref

		| CallFun(e, list, _) -> 
    			let comp_e = compile_expr' e in
    			let (args_comp, last_index) = 
    				List.fold_left( fun (prev_args, prev_index) e ->
    						let comp_e = compile_expr' e in
    						let new_args_comp = 
    							dup @ 
    							(ldc prev_index) @ 
    							comp_e @ 
    							stack_set @ 
    							prev_args in
    						let new_index = prev_index + 1 in
    							(new_args_comp, new_index)
    											) ([], 1) list in
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
    				call_fun

		| GetRecord(e, s, t) ->
					let comp_e = compile_expr' e in
					let var = (is_var t) in
					let deref = 
							if var && to_result then
								cell_get
							else
								[] in
						comp_e @ (ldstr s) @ record_get @ deref

		| GetArray(array, index, t) ->
					let comp_array = compile_expr' array in
					let comp_index = compile_expr' index in
					let var = (is_var t) in
					let deref = 
							if var && to_result then
								cell_get
							else
								[] in
						comp_array @ comp_index @ unbox_int32 @ array_get @ deref

(** ******************************************** STAT COMPILER ************************************************ *)

and compile_stat env s =
	let compile_expr' = compile_expr env true in
	let compile_stat' = compile_stat env in	
	match s with
		| Assign (l, r, _) ->
					let comp_l = compile_expr env false l in
					let comp_r = compile_expr' r in
						comp_l @ comp_r @ cell_set

		| Seq(l, r, _) ->
					let comp_l = compile_stat' l in
					let comp_r = compile_stat' r in
						comp_l @ comp_r
						
		| If(e, s, _) -> 
					let label = next_label () in
					let comp_e = compile_expr' e in
					let comp_s = compile_stat' s in
						comp_e @ unbox_bool @ (brfalse label) @ comp_s @ (nop label)
					
		| If_Else(e, s1, s2, _) -> 
					let label1 = next_label () in
					let label2 = next_label () in
					let comp_e = compile_expr' e in
					let comp_s1 = compile_stat' s1 in
					let comp_s2 = compile_stat' s2 in
						comp_e @ unbox_bool @ (brfalse label1) 
										@ comp_s1 @ (br label2) 
						@ (nop label1) 
										@ comp_s2 @ (nop label2)

		| While(e, s, _) -> 
					let label1 = next_label () in
					let label2 = next_label () in
					let comp_e = compile_expr' e in
					let comp_s = compile_stat' s in
						(	nop label1) @
							comp_e @ 
							unbox_bool @ 
							(brfalse label2) @ 
									comp_s @ 
							(br label1) @ 
							(nop label2)
		
		| Write (list, _) -> 
					List.fold_left (fun prev_comp e -> 
							let t = unref_iType (get_type e) in
							let comp_e = compile_expr' e in
							let comp_print = comp_e @ (write_type t) in
								prev_comp @ comp_print
													) [] list
		
		| WriteLn (list, t) -> 
					let comp_write = compile_stat' (Write (list, t)) in
					let print_ln = ("ldstr \"\n\"") :: (print "class System.String") in
						comp_write @ print_ln

		| CallProc (e, list, _) -> 
					let closure = compile_expr' e in
					let args_comp, num_args = 
							List.fold_left (fun (prev_comp, prev_index) e ->
									let comp_e = compile_expr' e in
									let new_index = prev_index + 1 in
									let new_comp = 
										dup @ (ldc new_index) @ comp_e @ stack_set @ prev_comp in
									(new_comp, new_index)
														) ([], 0) list in
					closure @ 
          dup @
          closure_get_SF @
          new_stack @ 
          dup @ 
          (ldc num_args) @ 
          stack_init_args @ 
          args_comp @ 
          swap @ 
          closure_get_Ftn @ 
          call_proc;;

let rec compile_oper env o =
	match o with
		| Function (name, args_list, [consts; vars; opers], s, t) -> 
					let id = fresh_identifier () in
					let comp_closure = ldloc_stackframe @ (ldftn_fun id) @ new_closure in
					inc_locals ();
					let new_env = assoc name env false in
					let _ , recursive_addr, _ = find name new_env in
					let fun_env = begin_scope new_env in
					begin_locals ();
					let args_env = 
							List.fold_left 	(fun prev_env (s, _) -> assoc s prev_env false
															) fun_env args_list in
					inc_locals ();
					let temp_env = assoc "result" args_env true in
					let _, result_addr, _ = find "result" temp_env in
					let comp_result = (ldloc_stackframe) @ (ldc result_addr) @ (compile_default_type t) @ stack_set in
					let get_result = ldloc_stackframe @ (ldc result_addr) @ stack_get @ cell_get in
					let decl_comp, decl_list, decl_env = compile_all_decls consts vars opers temp_env in
					let comp_s = compile_stat decl_env s in
					let num_locals = end_locals () in
					let comp_fun = (preamble_fun id num_locals) @ comp_result @ decl_comp @ comp_s @ get_result @ footer in
					let comp_set_closure = ldloc_stackframe @ (ldc recursive_addr) @ comp_closure @ stack_set in
						(comp_set_closure, comp_fun, new_env)

		| Procedure (name, args_list, [consts; vars; opers], s, _) -> 
					let id = fresh_identifier () in
					let comp_closure = ldloc_stackframe @ (ldftn_proc id) @ new_closure in
					inc_locals ();
					let new_env = assoc name env false in
					let _ , recursive_addr, _ = find name new_env in
					let proc_env = begin_scope new_env in
					begin_locals ();
					let args_env = 
							List.fold_left 	(fun prev_env (s, _) -> assoc s prev_env false
															) proc_env args_list in
					let decl_comp, decl_list, decl_env = compile_all_decls consts vars opers args_env in
					let comp_s = compile_stat decl_env s in
					let num_locals = end_locals () in
					let comp_proc = (preamble_proc id num_locals) @ decl_comp @ comp_s @ footer in
					let comp_set_closure = ldloc_stackframe @ (ldc recursive_addr) @ comp_closure @ stack_set in
						(comp_set_closure, comp_proc, new_env)

		| _ -> ([], [] ,[]) (* dummy *)

and compile_decl env d =
	match d with
		| Vars (list, _) ->
					let (vars_comp, vars_env) = 
						List.fold_left ( fun 	(prev_comp, prev_env) (t, list2) ->
								List.fold_left ( fun (prev_comp, prev_env) s -> 
										inc_locals ();
										let new_env = assoc s prev_env true in
										let _, addr, _ = find s new_env in
										let default_comp = compile_default_type t in
										let new_comp = prev_comp @ (ldloc_stackframe) @ (ldc addr) @ default_comp @ stack_set in
											(new_comp, new_env)
																) (prev_comp, prev_env) list2
													) ([], env) list in
						(vars_comp, [], vars_env)
		
		| Consts (list, _) ->
					let (consts_comp, consts_env) =
						List.fold_left ( fun 	(prev_comp, prev_env) (s, e) ->
									inc_locals ();
									let new_env = assoc s prev_env false in
									let _, addr, _ = find s new_env in
									let comp_e = compile_expr env true e in
									let new_comp = prev_comp @ (ldloc_stackframe) @ (ldc addr) @ comp_e @ stack_set in
										(new_comp, new_env)
												) ([], env) list in
						(consts_comp, [], consts_env)

		| Operations (list, _) -> 
					List.fold_left ( fun 	(prev_comp, prev_list, prev_env) o ->
								let oper_comp, oper_list, oper_env = compile_oper prev_env o in
								 (prev_comp @ oper_comp, prev_list @ oper_list, oper_env)
													) ([], [], env) list

and compile_all_decls consts vars opers env =
	let (consts_comp, _, consts_env) = compile_decl env consts in
	let (vars_comp, _, vars_env) = compile_decl consts_env vars in
	let (opers_comp, opers_list, opers_env) = compile_decl vars_env opers in
		consts_comp @ vars_comp @ opers_comp, opers_list, opers_env

let rec optimize comp =
	comp;;

let compile_program p =
	match p with
		| Program (name, [consts; vars; opers], s, _) ->
					let env = begin_scope [] in
					begin_locals ();
					let decl_comp, decl_list, decl_env = compile_all_decls consts vars opers env in
					let comp_s = compile_stat decl_env s in
					let all_comp = decl_comp @ comp_s in
					let optimized_s = optimize all_comp in
					let optimized_opers = optimize decl_list in
					let num_locals = end_locals () in
						(preamble num_locals)@ optimized_s @ footer @ optimized_opers
		| _ -> [] (* dummy *);;
