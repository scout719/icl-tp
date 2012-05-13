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
			(* print_string ("Terminal:\n"); (evalProgram s); (print_string "\n"); *)
			(* compile_program s *)
			print_string (string_of_iType (get_type_program (typechk_program s)))
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
