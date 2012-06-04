open Blaise_parser
open Blaise_syntax
open Blaise_semantics
open Blaise_compiler
open Blaise_typechk
open Blaise_iType

let rec prompt comm lexbuf =
	flush stdout;
  try
  	let s = Blaise_parser.main Blaise_lexer.token lexbuf in
  		let p = typechk_program s in
  			if not_none (get_type_program p) then (
  				if comm = "-i" then (
  					print_string "Terminal:\n"; evalProgram p
  				)else if comm = "-c" then (
  					List.iter (fun s -> print_string (s ^ "\n")) (compile_program p)
  				)else if comm = "-u" then (
  					print_string (unparse_program p)
  				) else (
  					print_string "Invalid option\n"
  				)
  			) else (
  				print_string "Invalid Program:\n";
  			)
  with
   	Parsing.Parse_error -> print_string "Parsing error\n"
  | End_of_file -> ()
;;

let main () =
	let args = Sys.argv in
		if Array.length args = 3 then (
			let lexbuf = Lexing.from_channel (open_in(args.(2))) in prompt (args.(1)) lexbuf
		) else if Array.length args < 2 or Array.length args > 3 or (args.(1)) = "-help" then (
    	print_string "Available commands:\n./main.native OPTION [filename]\n\t-i -> Interpreter\n\t-c -> Compiler\n\t-u -> Unparser\n\t-help -> Help\n"
		) else (
			let lexbuf = Lexing.from_channel stdin in prompt (args.(1)) lexbuf
		)
		
;;

main();;
