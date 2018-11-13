open Core

let pda_to_spda pda =
  let get_next rem_arr =
    match (Word.hd rem_arr.Pda.Arrow.pops, Word.hd rem_arr.pushes) with
    | _ when rem_arr.consume <> Letter.epsilon ->
      Some (
        Spda.Arrow.Consume rem_arr.consume,
        { rem_arr with consume = Letter.epsilon; }
      )
    | (pop, _) when pop <> Letter.epsilon ->
      Some (
        Spda.Arrow.Pop pop,
        { rem_arr with pops = Word.tl_exn rem_arr.pops; }
      )
    | (_, psh) when psh <> Letter.epsilon ->
      Some (
        Spda.Arrow.Push psh,
        { rem_arr with pushes = Word.tl_exn rem_arr.pushes; }
      )
    | (_, _) -> None in
  let spda = Spda.new_empty () in
  Array.iter pda.Pda.states ~f: (fun st ->
    let _ = Spda.add_empty_state spda st.is_accept in ()
  );
  Array.iteri pda.Pda.states ~f: (fun src st ->
    Array.iteri st.transitions ~f: (fun dst tr ->
      let rec loop cur_src rem_arr next1 =
        match next1 with
        | None ->
          (* Only happens in base case *)
          assert (phys_equal src cur_src);
          Spda.add_transition spda (cur_src, Spda.Arrow.Epsilon, dst)
        | Some (next_arr, next_rem_arr) ->
          let next2 = get_next next_rem_arr in
          match next2 with
          | None ->
            Spda.add_transition spda (cur_src, next_arr, dst)
          | Some next2 ->
            let int = Spda.add_empty_state spda false in
            Spda.add_transition spda (cur_src, next_arr, int);
            loop int next_rem_arr (Some next2) in
      tr.arrows |>
      List.rev |>
      List.iter ~f: (fun arr ->
        loop src arr (get_next arr)
      )
    )
  );
  !spda

let cfg_to_pda cfg =
  let pda = Pda.new_empty () in
  (* Initial state *)
  let _ = Pda.add_empty_state pda false in
  (* Derivations state *)
  let _ = Pda.add_empty_state pda true in
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
    Array.iteri st1.transitions ~f: (fun q2 tr ->
      let spda2 = Spda.copy spda in
      Spda.swap_ids spda2 0 q2;
      if z1 = Letter.epsilon then
        List.iter tr.arrows ~f: (fun arr ->
          match arr with
          | Spda.Arrow.Epsilon ->
            let cfg2 = spda_to_cfg !spda2 in
            let v2 = Cfg.add_child_var cfg cfg2 in
            Cfg.add_derivation cfg v1 (Cfg.Derivation.NonTerminal [v2])
          | Consume csm ->
            let v2 = Cfg.have_var cfg { Cfg.Var.derivations = [Terminal csm]; }
            and cfg2 = spda_to_cfg !spda2 in
            let v3 = Cfg.add_child_var cfg cfg2 in
            Cfg.add_derivation cfg v1 (Cfg.Derivation.NonTerminal [v2; v3])
          | Push psh ->
            Array.iteri spda.states ~f: (fun q3 _ ->
              Spda.set_accept_only spda2 q3;
              let cfg2 = spda_to_cfg !spda2 in
              let v2 = Cfg.add_child_var cfg cfg2
              and v3 = Cfg.add_var cfg Cfg.Var.empty in
              loop q3 v3 psh;
              Cfg.add_derivation cfg v1 (Cfg.Derivation.NonTerminal [v2; v3])
            );
          | Pop _ -> ()
        )
      else if List.exists tr.arrows ~f: (fun arr -> arr = Spda.Arrow.Pop z1) then
        let cfg2 = spda_to_cfg !spda2 in
        let v2 = Cfg.add_child_var cfg cfg2 in
        Cfg.add_derivation cfg v1 (Cfg.Derivation.NonTerminal [v2])
    ) in
  loop 0 0 Letter.epsilon;
  !cfg

let pda_to_cfg pda =
  spda_to_cfg (pda_to_spda pda)
