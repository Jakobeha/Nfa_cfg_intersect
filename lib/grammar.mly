(* Derived from https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html *)
%{
open Core
%}
%token <Letter.t> Letter
%token <Id.t> Id
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
%start <Spda.t> spda
%start <Nfa.t> nfa
%%
cfg:
  | vars = separated_nonempty_list(Newline, cfg_var); Eof
  { Cfg.parse vars }
  ;
cfg_var:
  | id = Id; Space; Dash; Arrow_head; Space; dvs = separated_nonempty_list(Bar, cfg_derivation)
  { (id, { Cfg.Var.derivations = dvs }) }
  ;
cfg_derivation:
  | l = Letter
  { Cfg.Derivation.Terminal l }
  | vs = list(Id)
  { Cfg.Derivation.NonTerminal vs }
  ;
pda:
  | sts = automaton(pda_arrow); Eof
  { Pda.parse sts }
  ;
spda:
  | sts = automaton(spda_arrow); Eof
  { Spda.parse sts }
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
  | id = Id; acp = accept; Space; Dash; Star
  { (id, acp, []) }
  | id = Id; acp = accept; Space; Dash; t1 = state_transition(arrow); ts = list(state_extra(arrow))
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
  | arr = arrow; Arrow_head; Space; dst = Id
  { (dst, arr) }
pda_arrow:
  | Open_brace; csm = Letter; Comma; pops = nonempty_list(free_letter); Dash; Arrow_head; pshs = nonempty_list(free_letter); Close_brace; Dash
  { { Pda.Arrow.consume = csm; pops = Word.of_letters pops; pushes = Word.of_letters pshs; } }
spda_arrow:
  | Open_brace; x = spda_arrow_content; Close_brace; Dash
  { x }
%inline spda_arrow_content:
  | csm = free_letter
  { if csm = Letter.epsilon then Spda.Arrow.Epsilon else Consume csm }
  | Plus; psh = free_letter
  { Spda.Arrow.Push psh }
  | Dash; pop = free_letter
  { Spda.Arrow.Pop pop }
%inline free_letter:
  | a = Letter
  { a }
  | a = Id
  { Id.to_letter a }
nfa_arrow:
  | csm = Letter; Dash
  { { Nfa.Arrow.consume = csm; } }
