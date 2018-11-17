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
            pops = Word.of_letter (Id.to_letter idx);
            pushes = "";
          }
        | NonTerminal vs ->
          { Pda.Arrow.consume = Letter.epsilon;
            pops = Word.of_letter (Id.to_letter idx);
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

let spda_to_cfg spda =
  (* For each pair of states p and q, create a variable Apq, to get from
     p to q on any stack without changing it. This can be done either
     "direct" (consume a letter or epsilon), "hill" (push a letter,
     consume another variable, and pop the same letter), "transitive"
     (consume Apr and Arq), or "reflexive" (if p = q, consume epsilon).
     Then, the start variable derives to all A0q, where q is an accept
     state (gets from start to accept on an empty stack). *)
  let num_sts = Array.length spda.Spda.states in
  let get_vid q1 q2 =
    (q1 * num_sts) + q2 + 1 in
  { Cfg.variables =
      Array.init ((num_sts * num_sts) + 1) ~f: (fun vid ->
        if vid = 0 then
          (* start state - derives to A0i where i is accept *)
          { Cfg.Var.derivations =
              spda.states |>
              Array.to_list |>
              List.mapi ~f: (fun q st -> (q, st)) |>
              List.filter ~f: (fun (_, st) -> st.Spda.State.is_accept) |>
              List.map ~f: (fun (q, _) -> Cfg.Derivation.NonTerminal [get_vid 0 q]);
          }
        else
          let q12 = vid - 1 in
          let q1 = q12 / num_sts
          and q2 = q12 mod num_sts in
          let st1 = Array.get spda.states q1 in
          let tr12 = Array.get st1.transitions q2 in
          { Cfg.Var.derivations =
              (* direct - consume a letter or epsilon *)
              List.filter_map tr12.arrows ~f: (fun arr12 ->
                match arr12 with
                | Spda.Arrow.Push _
                | Pop _ -> None
                | Epsilon -> Some (Cfg.Derivation.Terminal Letter.epsilon)
                | Consume l -> Some (Terminal l)
              ) @
              (* hill - push a letter, consume a variable, then pop *)
              List.filter_opt (List.init (num_sts * num_sts) (fun qi1i2 ->
                let qi1 = qi1i2 / num_sts
                and qi2 = qi1i2 mod num_sts in
                let sti2 = Array.get spda.states qi2 in
                let tr1i1 = Array.get st1.transitions qi1
                and tri22 = Array.get sti2.transitions q2 in
                let pushes =
                  List.filter_map tr1i1.arrows ~f: (fun arr1i1 ->
                    match arr1i1 with
                    | Spda.Arrow.Epsilon
                    | Consume _
                    | Pop _ -> None
                    | Push l -> Some l
                  )
                and pops =
                  List.filter_map tri22.arrows ~f: (fun arri22 ->
                    match arri22 with
                    | Spda.Arrow.Epsilon
                    | Consume _
                    | Push _ -> None
                    | Pop l -> Some l
                  ) in
                if List.exists pushes ~f: (fun push ->
                  List.exists pops ~f: (fun pop ->
                    push = pop
                  )
                ) then
                  Some (Cfg.Derivation.NonTerminal [get_vid qi1 qi2])
                else
                  None
              )) @
              (* transitive - forall r, consume Apr and Arq
                 reflexive - if p = q, consume epsilon *)
              ( if q1 = q2 then
                  [Cfg.Derivation.Terminal Letter.epsilon]
                else
                  List.init num_sts (fun q3 ->
                    Cfg.Derivation.NonTerminal [get_vid q1 q3; get_vid q3 q2]
                  )
              );
          }
      )
  }
