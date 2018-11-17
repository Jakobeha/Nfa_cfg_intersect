open Core

let intersect_spda_nfa spda nfa =
  (* Create a state for each pair of states p and q. For every epsilon
     transition from p1 to p2 or q1 to q2, add the same transition from
     (p1, q) to (p2, q) or (p, q1) to (p, q2), preserving the PDA's
     pushes and pops. For every transition consuming a from both p1 to
     p2 and q1 to q2, add the same transition from (p1, p2) to (q1, q2).
     Start at (p0, q0), accept iff both states accept. *)
  { Spda.states =
      Array.concat_mapi spda.Spda.states ~f: (fun idp stp ->
        Array.mapi nfa.Nfa.states ~f: (fun idn stn ->
          { Spda.State.transitions =
              Array.concat_mapi stp.transitions (fun dstp trp ->
                Array.mapi stn.transitions (fun dstn trn ->
                  { Spda.Transition.arrows =
                      (* epsilon from p1 to p2 - from (p1, q) to (p2, q) *)
                      ( if idn = dstn then
                          List.filter trp.arrows ~f: (fun arrp ->
                            match arrp with
                            | Consume _ -> false
                            | Epsilon
                            | Push _
                            | Pop _ -> true
                          )
                        else
                          []
                      ) @
                      (* epsilon from q1 to q2 - from (p, q1) to (p, q2) *)
                      ( if idp = dstp && List.exists trn.arrows ~f: (fun arrn ->
                          arrn.consume = Letter.epsilon
                        ) then
                          [Spda.Arrow.Epsilon]
                        else
                          []
                      ) @
                      (* consume a both from p1 to p2 and from q1 to q2 -
                         from (p1, q1) to (p2, q2) *)
                      List.filter trp.arrows ~f: (fun arrp ->
                        match arrp with
                        | Consume l ->
                          List.exists trn.arrows ~f: (fun arrn ->
                            arrn.consume = l
                          )
                        | Epsilon
                        | Push _
                        | Pop _ -> false
                      );
                  }
                )
              );
            is_accept = stp.is_accept && stn.is_accept;
          }
        )
      );
  }

let intersect_cfg_nfa cfg nfa =
  cfg |>
  Convert.cfg_to_pda |>
  Convert.pda_to_spda |>
  (fun spda -> intersect_spda_nfa spda nfa) |>
  Convert.spda_to_cfg
