open Blaise_parser
open Blaise_syntax
open Blaise_semantics
open Blaise_compiler
open Blaise_typechk
open Blaise_iType

let rec prompt lexbuf =
(*	print_string "> " ;*)
	flush stdout;
  try
  	let s = Blaise_parser.main Blaise_lexer.token lexbuf in
			let p = typechk_program s in
				if not_none (get_type_program p) then (
					List.iter (fun s -> print_string (s ^ "\n")) (compile_program p)
					(* print_string (unparse_program p); *)
    			(* print_string "Terminal:\n"; evalProgram p *)
				) else (
					print_string "Invalid Program:\n";
					print_string (unparse_program p)
				)
			
			(* print_string (unparse_program s) *)

			(* print_string (unparse_program (typechk_program s)) *)
			
			(* print_string "Terminal:\n"; evalProgram s; print_string "\n"; *)
			
			(* print_string (string_of_iType (get_type_program (typechk_program s))) *)
			
			(* prompt lexbuf *)
  with
   	Parsing.Parse_error -> print_string "Parsing error\n"; (* prompt lexbuf *) 
	| End_of_file -> ()
;;

let main () =
	let args = Sys.argv in
		if Array.length args = 2 then
			let lexbuf = Lexing.from_channel (open_in(Sys.argv.(1))) in prompt lexbuf
		else
			let lexbuf = Lexing.from_channel stdin in prompt lexbuf
;;

main();;
