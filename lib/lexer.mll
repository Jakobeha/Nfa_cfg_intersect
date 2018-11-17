(* Derived from https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html *)
{
open Core
open Lexing
open Grammar

exception SyntaxError of string
}

rule read = parse
  | '_' { Letter Letter.epsilon }
  | ['a'-'z'] (['1'-'9'] ['0'-'9']+)? { Letter (Letter.lex (Lexing.lexeme lexbuf)) }
  | ['A'-'Z'] (['1'-'9'] ['0'-'9']+)? { Id (Id.lex (Lexing.lexeme lexbuf)) }
  | '\n' eof { Eof }
  | "\n===\n" { Separator }
  | "\n   " { Newline_3_space }
  | ' ' { Space }
  | '\n' { Newline }
  | '@' { At }
  | '|' { Bar }
  | '-' { Dash }
  | '+' { Plus }
  | '*' { Star }
  | '>' { Arrow_head }
  | '[' { Open_brace }
  | ']' { Close_brace }
  | ',' { Comma }
  | eof { Eof }
  | _  { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
