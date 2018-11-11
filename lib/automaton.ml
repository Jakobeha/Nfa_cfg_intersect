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
      List.map ~f: (fun arr -> Arrow.print arr ^ "-> " ^ Id.print idx) tr.arrows
  end

  module State = struct
    type t =
      { transitions : Transition.t Array.t;
        is_accept : Bool.t;
      }

    let parse num_sts acp trs =
      let trs = Array.init num_sts ~f: (fun dst ->
        { Transition.arrows = trs |>
            List.filter ~f: (fun (tdst, _) -> tdst = dst) |>
            List.map ~f: (fun (_, arr) -> arr)
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
            List.concat_mapi ~f: Transition.print_lines (Array.to_list st.transitions)
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
    let new_num_sts = Array.length !atm.states + 1 in
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
    }

  let add_transition (src, arr, dst) atm =
    let st = Array.get atm.states src in
    let ts = Array.get st.transitions dst in
    let ts = { Transition.arrows = arr :: ts.arrows; } in
    Array.set st.transitions dst ts

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
