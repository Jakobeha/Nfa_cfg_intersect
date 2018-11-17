(* Derived from https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html *)
open Core
open Lexing
open Lexer
open Grammar

let cfg_and_nfa inp =
  Grammar.cfg_and_nfa_eof Lexer.read inp

let cfg inp =
  Grammar.cfg_eof Lexer.read inp

let pda inp =
  Grammar.pda_eof Lexer.read inp

let spda inp =
  Grammar.spda_eof Lexer.read inp

let nfa inp =
  Grammar.nfa_eof Lexer.read inp
