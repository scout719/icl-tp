open Blaise_parser
open Blaise_syntax
open Blaise_semantics

let rec prompt lexbuf =
	print_string "> " ;
	flush stdout;
  try
  	let s = Blaise_parser.main Blaise_lexer.token lexbuf in
			print_string ("Terminal:\n"); (evalProgram s);(print_string "\n");
			prompt lexbuf
  with
   	Parsing.Parse_error -> print_string "Parsing error\n"; prompt lexbuf 
	| End_of_file -> ()
;;

let main () =
  let lexbuf = Lexing.from_channel (stdin) in prompt lexbuf
;;

main();;