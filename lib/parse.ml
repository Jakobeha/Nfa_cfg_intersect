(* Derived from https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html *)
open Core
open Lexing
open Lexer
open Grammar

exception Error of Error.t

let cfg inp =
  Grammar.cfg Lexer.read inp

let pda inp =
  Grammar.pda Lexer.read inp

let spda inp =
  Grammar.spda Lexer.read inp

let nfa inp =
  Grammar.nfa Lexer.read inp
