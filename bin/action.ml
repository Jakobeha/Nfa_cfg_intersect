open Core

let intersect_nfa_cfg () =
  try
    let (cfg, nfa) = Lib.Parse.cfg_and_nfa (Lexing.from_channel In_channel.stdin) in
    let res = Lib.Operate.intersect_cfg_nfa cfg nfa in
    Out_channel.prerr_endline "  ";
    Out_channel.print_endline (Lib.Cfg.print res)
  with
  | Lib.Lexer.SyntaxError msg ->
    Out_channel.prerr_endline "Lexing error:";
    Out_channel.prerr_endline msg
  | Lib.Grammar.Error ->
    Out_channel.prerr_endline "Grammar error"
  | exn ->
    Out_channel.prerr_endline "Error:";
    Out_channel.prerr_endline (Exn.to_string exn)
