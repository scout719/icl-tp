{
open Blaise_parser
}

let digit = ['0'-'9']
let char = ['a'-'z' 'A'-'Z']
let id = "_"* char (char | digit | "_")*


let  dec_digit = ['0' - '9']
let  dec_nonzero = ['1' - '9']
let  dec_constant = dec_digit+
let  hex_digit = dec_digit | ['a' - 'f'] | ['A' - 'F']
let  hex_constant = '0' ('x' | 'X') hex_digit+
let  oct_digit = ['0' - '7']
let  oct_constant = '0' oct_digit+
let  quote = '\''
let  escapable_char = '\\' | ' ' | quote | '.' | '#' | '\"' | 'n' | 't' | 'r' | 'b' | 'f'
let  escape_code = 'u' hex_digit hex_digit hex_digit hex_digit
let  escape_char = '\\' (escapable_char | escape_code)
let  string_char = escape_char | ['\000' - '\033'] | ['\035' - '\091'] | ['\093' - '\127']
let  string_constant = '"' string_char* '"'

let  block_comment_char = escape_char | ['\000' - '\041'] | ['\043' - '\046'] | ['\048' - '\127']
let  inline_comment_char = escape_char | ['\000' - '\009'] | ['\014' - '\046'] | ['\048' - '\127']

let  block_comment = "/*" (block_comment_char | "*" (block_comment_char) | "/")* "*/"
let  inline_comment = "//" (inline_comment_char | ("/" inline_comment_char))* ['\010' - '\013']


rule token = parse
	| block_comment { token lexbuf }
	| inline_comment { token lexbuf }
	| [' ' '\t' '\r' '\n'] { token lexbuf }
	| "program" { PROGRAM }
	| ";;" { EOF }
	| "function" { FUNCTION }
	| "procedure" { PROCEDURE }
	| "begin" { BEGIN }
	| "end" { END }
	| "while" { WHILE }
	| "do" { DO }
	| "if" { IF }
	| "then" { THEN }
	| "else" { ELSE }
	| "var" { VAR }
	| "const" { CONST }
	| "not" { NOT }
	| "true" { TRUE }
	| "false" { FALSE }
	| "write" { WRITE }
	| "writeln" { WRITELN }
	| "result" { RESULT }
	| "read" { READ }
	| "readln" { READLN }
	| "Integer" { INTEGER }
	| "Bool" { BOOL }
	| "String" { STRING }
	| "Array" { ARRAY }
	| "Record" { REC }
	| "Fun" { FUN }
	| "Class" { TCLASS }
	| "Object" { TOBJECT }
	| "new" { NEW }
	| "Proc" { PROC }
	| "!quit" { QUIT }
	| "class" { CLASS }
	| ':' { COLON }
	| '(' { LPAR }
	| ')' { RPAR } 
	| '[' { LSBRA }
	| ']' { RSBRA }
	| '{' { LCBRA }
	| '}' { RCBRA }
	| ';' { SEMICOLON }
	| ',' { COMMA }
	| '.' { DOT }
	| '+' { PLUS } 
	| '-' { MINUS } 
	| '*' { MULT } 
	| '/' { DIV } 
	| '%' { MOD }
	| '=' { EQUAL }
	| "<>" { NEQ }
	| '>' { GT }
	| ">=" { GTEQ }
	| '<' { LT }
	| "<=" { LTEQ }
	| "and" { AND }
	| "or" { OR }
 	| ":=" { ASSIGN }
	| digit+ as num { NUM (int_of_string num) }
	| id as word { ID word }
	| string_constant as string { 
      let s = String.sub string 1 (String.length string - 2) in
      STRING_CONST s }
	| eof { EOF }
	| _ { token lexbuf }