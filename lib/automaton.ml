open Core

module Checkpoint = struct
  type 'mem t =
    | Reject
    | Continue of 'mem * int
end

module Make (Memory : sig
  type t

  val init : t

  val can_accept : t -> Bool.t
end)(Arrow : sig
  type t

  val follow : Memory.t -> Letter.t -> t -> Memory.t Checkpoint.t

  val print : t -> string
end) = struct
  module Transition = struct
    type t =
      { arrows : Arrow.t List.t;
      }

    let empty =
      { arrows = [];
      }

    let print_lines idx tr =
      (* reverses so transitions are added at the end *)
      List.rev_map ~f: (fun arr -> Arrow.print arr ^ "-> " ^ Id.print idx) tr.arrows
  end

  module State = struct
    type t =
      { transitions : Transition.t Array.t;
        is_accept : Bool.t;
      }

    let copy st =
      { transitions = Array.copy st.transitions;
        is_accept = st.is_accept;
      }

    let parse num_sts acp trs =
      let trs = Array.init num_sts ~f: (fun dst ->
        { Transition.arrows = trs |>
            List.filter ~f: (fun (tdst, _) -> tdst = dst) |>
            (* reverses so transitions are added at the end *)
            List.rev_map ~f: (fun (_, arr) -> arr)
        }
      ) in
      { transitions = trs;
        is_accept = acp;
      }

    let print idx st =
      Id.print idx ^
      (if st.is_accept then "@" else " ") ^
      " -" ^ (
        if Array.for_all ~f: (fun tr -> List.is_empty tr.arrows) st.transitions then
          "*"
        else
          String.concat ~sep: "\n   +" (
            st.transitions |>
            Array.to_list |>
            List.concat_mapi ~f: Transition.print_lines
          )
      )
  end

  module Frame = struct
    type t =
      { state_idx : Id.t;
        word_idx : Int.t;
        memory : Memory.t;
        visited : Id.Set.t;
      }

    let init =
      { state_idx = 0;
        word_idx = 0;
        memory = Memory.init;
        visited = Id.Set.empty;
      }
  end

  type t =
      { states : State.t Array.t
      }

  let new_empty () = ref
    { states = [||]
    }

  let add_empty_state atm acp =
    let new_id = Array.length !atm.states in
    let new_num_sts = new_id + 1 in
    let st =
      { State.transitions = Array.init new_num_sts ~f: (fun _ -> Transition.empty);
        is_accept = acp;
      } in
    atm := { states = Array.append (
        Array.map ~f: (fun st ->
          { State.transitions = Array.append st.transitions [|Transition.empty|];
            is_accept = st.is_accept;
          }
        ) !atm.states
      ) [|st|];
    };
    new_id

  let add_transition atm (src, arr, dst) =
    let st = Array.get !atm.states src in
    let tr = Array.get st.transitions dst in
    let tr = { Transition.arrows = arr :: tr.arrows; } in
    Array.set st.transitions dst tr

  let clear_transition atm src dst =
    let st = Array.get !atm.states src in
    let trs = st.transitions in
    Array.set trs dst Transition.empty


  let copy atm = ref
    { states = Array.map ~f: State.copy atm.states;
    }

  (** Switches the ids of the 2 states.
      e.g. switch a state's id to 0 to make it the new start state *)
  let swap_ids atm id1 id2 =
    Array.iter !atm.states ~f: (fun st ->
      Array.swap st.transitions id1 id2
    );
    Array.swap !atm.states id1 id2

  let set_accept_only atm id =
    Util.Array.mapi_inplace !atm.states ~f: (fun id2 st2 ->
      { st2 with State.is_accept = id = id2; }
    )

  let run atm w =
    let rec next fr =
      let st = Array.get atm.states fr.Frame.state_idx
      and at_end = fr.word_idx = String.length w in
      if at_end && Memory.can_accept fr.memory && st.is_accept then
        Run_result.Accept
      else
        let rej = Run_result.Reject fr.word_idx
        and l =
          if at_end then
            Letter.epsilon
          else
            Word.get w fr.word_idx in
        Array.mapi st.transitions ~f: (fun ist2 tr ->
          if Id.Set.mem fr.visited ist2 then
            rej
          else
            List.map tr.arrows ~f: (fun arr ->
              match Arrow.follow fr.memory l arr with
              | Reject -> rej
              | Continue (mem2, adv) ->
                let iw2 = fr.word_idx + adv
                and vst2 =
                  if adv > 0 || mem2 <> fr.memory then
                    Id.Set.empty
                  else
                    Id.Set.add fr.visited fr.state_idx in
                assert (iw2 <= String.length w);
                next
                  { state_idx = ist2;
                    word_idx = iw2;
                    memory = mem2;
                    visited = vst2;
                  }
            ) |>
            List.min_elt ~compare: Run_result.compare |>
            Option.value ~default: rej
        ) |>
        Array.min_elt ~compare: Run_result.compare |>
        Option.value ~default: rej in
    next Frame.init

  let parse sts =
    assert (not (List.is_empty sts));
    let num_sts = List.length sts in
    { states = sts |>
        List.map ~f: (fun (id, acp, trs) -> (id, State.parse num_sts acp trs)) |>
        Util.Id.validate_order |>
        Array.of_list;
    }

  let print atm =
    String.concat ~sep: "\n" (Array.to_list (Array.mapi ~f: State.print atm.states))
end
