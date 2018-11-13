open Core

module Make (Arrow : sig
  type t

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
    let ts = Array.get st.transitions dst in
    let ts = { Transition.arrows = arr :: ts.arrows; } in
    Array.set st.transitions dst ts

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

  let parse sts =
    assert (not (List.is_empty sts));
    let num_sts = List.length sts in
    { states = sts |>
        List.map ~f: (fun (id, acp, trs) -> (id, State.parse num_sts acp trs)) |>
        Util.validate_id_order |>
        Array.of_list;
    }

  let print atm =
    String.concat ~sep: "\n" (Array.to_list (Array.mapi ~f: State.print atm.states))
end
