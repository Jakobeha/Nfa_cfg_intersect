open Core

let cfg_to_pda cfg =
  let pda = Pda.new_empty () in
  (* Initial state *)
  Pda.add_empty_state pda false;
  (* Derivations state *)
  Pda.add_empty_state pda true;
  let start_id = 0
  and mid_id = 1 in
  (* Push the start state onto the stack *)
  Pda.add_transition pda (
    start_id,
    { Pda.Arrow.consume = Letter.epsilon; pops = ""; pushes = "S"; },
    mid_id
  );
  (* For every possible derivation, add a transition where the PDA
     "chooses" the derivation and follows it. *)
  Array.iteri ~f: (fun idx var ->
    List.iter ~f: (fun dv ->
      let arr =
        match dv with
        | Cfg.Derivation.Terminal l ->
          { Pda.Arrow.consume = l;
            pops = Letter.to_string (Id.to_letter idx);
            pushes = "";
          }
        | NonTerminal vs ->
          { Pda.Arrow.consume = Letter.epsilon;
            pops = Letter.to_string (Id.to_letter idx);
            pushes =
              vs |>
              List.rev_map ~f: Id.to_letter |>
              Word.of_letters;
          } in
      Pda.add_transition pda (mid_id, arr, mid_id)
    ) var.derivations
  ) cfg.Cfg.variables;
  (* Generate a state for every variable *)
  !pda

let rec spda_to_cfg spda =
  let cfg = Cfg.new_empty () in
  (* Follow the automata's transitions, keeping track of the automata's
     state q1, CFG's current variable A, and a "held" variable z. Start
     with q1 = the start state, A = S, and z = 0 (epsilon). When the
     automata transitions to q2, consuming a (can be epsilon), pushing x
     (can be epsilon), and *popping z* - if x is epsilon, add the
     derivation A -> aB, where B is the CFG for the same PDA, but
     starting at q2 (also has the same accept states) | if x isn't
     epsilon, for every state (including q1) q3, add the derivation
     A -> aBC, where B is the CFG for the same PDA, but starting at q2
     and accepting only q3, and also recurse adding derivations with
     q1 = q3, A = C, and z = x. *)
  let rec loop q1 v1 z1 =
    let st1 = Array.get spda.Spda.states q1 in
    Array.iteri st1.transitions ~f: (fun q2 tr1 ->
      tr1.arrows |>
      List.filter ~f: (fun arr1 -> arr1.pop = z1) |>
      List.iter ~f: (fun arr1 ->
        let v2 = Cfg.have_terminal_var cfg arr1.consume
        and spda2 = Pda.copy spda in
        Pda.switch_start spda2 q2;
        if arr1.push = Letter.epsilon then
          let cfg2 = spda_to_cfg spda2 in
          let v3 = Cfg.add_child_var cfg2 in
          Cfg.add_derivation cfg v1 (Cfg.Derivation.NonTerminal [v2; v3]);
        else
          Array.iteri spda.states ~f: (fun q3 _ ->
            Spda.set_accept_only spda q3;
            let cfg2 = spda_to_cfg spda2 in
            let v3 = Cfg.add_child_var cfg cfg2
            and v4 = Cfg.add_empty_var cfg in
            loop q3 v4 arr1.push;
            Cfg.add_derivation cfg v1 (Cfg.Derivation.NonTerminal [v2; v3; v4])
          );
      );
    ) in
  loop 0 0 Letter.epsilon;
  !cfg

let pda_to_cfg pda =
  spda_to_cfg (pda_to_spda pda)
