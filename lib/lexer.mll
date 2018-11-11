(* Derived from https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html *)
{
open Core
open Lexing
open Grammar

exception SyntaxError of string

let lexeme_char lexbuf =
  Char.of_string (Lexing.lexeme lexbuf)
}

rule read = parse
  | ['a'-'z' '0'] { Letter (lexeme_char lexbuf) }
  | ['A'-'Z'] { Ident (Id.lex (lexeme_char lexbuf)) }
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
