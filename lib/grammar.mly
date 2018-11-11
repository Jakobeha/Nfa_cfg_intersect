(* Derived from https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html *)
%{
open Core
%}
%token <Letter.t> Letter
%token <Id.t> Ident
%token Newline_3_space
%token Space
%token Newline
%token At
%token Bar
%token Dash
%token Plus
%token Star
%token Arrow_head
%token Open_brace
%token Close_brace
%token Comma
%token Eof
%start <Cfg.t> cfg
%start <Pda.t> pda
%start <Nfa.t> nfa
%%
cfg:
  | vars = separated_nonempty_list(Newline, cfg_var); Eof
  { Cfg.parse vars }
  ;
cfg_var:
  | id = Ident; Space; Dash; Arrow_head; Space; dvs = separated_nonempty_list(Bar, cfg_derivation)
  { (id, { Cfg.Var.derivations = dvs }) }
  ;
cfg_derivation:
  | l = Letter
  { Cfg.Derivation.Terminal l }
  | vs = nonempty_list(Ident)
  { Cfg.Derivation.NonTerminal vs }
  ;
pda:
  | sts = automaton(pda_arrow); Eof
  { Pda.parse sts }
  ;
nfa:
  | sts = automaton(nfa_arrow); Eof
  { Nfa.parse sts }
  ;
automaton(arrow):
  | sts = separated_nonempty_list(Newline, state(arrow))
  { sts }
  ;
state(arrow):
  | id = Ident; acp = accept; Space; Dash; Star
  { (id, acp, []) }
  | id = Ident; acp = accept; Space; Dash; t1 = state_transition(arrow); ts = list(state_extra(arrow))
  { (id, acp, t1 :: ts) }
  ;
%inline accept:
  | Space
  { false }
  | At
  { true }
  ;
state_extra(arrow):
  | Newline_3_space; Plus; t = state_transition(arrow)
  { t }
  ;
%inline state_transition(arrow):
  | arr = arrow; Arrow_head; Space; dst = Ident
  { (dst, arr) }
pda_arrow:
  | Open_brace; csm = Letter; Comma; pops = nonempty_list(free_letter); Dash; Arrow_head; pshs = nonempty_list(free_letter); Close_brace; Dash
  { { Pda.Arrow.consume = csm; pops = Word.of_letters pops; pushes = Word.of_letters pshs; } }
%inline free_letter:
  | a = Letter
  { a }
  | a = Ident
  { Id.to_letter a }
nfa_arrow:
  | csm = Letter; Dash
  { { Nfa.Arrow.consume = csm; } }
