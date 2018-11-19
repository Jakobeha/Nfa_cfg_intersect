(** Context free grammar.
    Not necessarily in Chomsky Normal Form, but limited in that
    derivations must contain 1 terminal (can be epsilon)
    or a list of variables (empty list = epsilon terminal). *)

open Core

module Term = struct
  type t =
    | Letter of Letter.t
    | Variable of Id.t

  let to_letter trm =
    match trm with
    | Letter l -> l
    | Variable id -> Lib.Id.to_letter id
end

module Derivation = struct
  type t =
    | Terminal of Letter.t
    | NonTerminal of Id.t List.t

  let is_epsilon drv =
    match drv with
    | Terminal l -> l = Letter.epsilon
    | NonTerminal dids -> List.is_empty dids

  let print d =
    match d with
    | Terminal l -> Letter.print l
    | NonTerminal vs ->
      if List.is_empty vs then
        Letter.print Letter.epsilon
      else
        String.concat (List.map ~f: Id.print vs)

end

module Var = struct
  type t =
    { derivations : Derivation.t List.t;
    }

  let empty =
    { derivations = [];
    }

  (* Removes derivations containing the id *)
  let elim var did =
    { derivations =
        List.filter var.derivations ~f: (fun dv ->
          match dv with
          | Derivation.Terminal _ -> true
          | Derivation.NonTerminal ids ->
            List.for_all ids ~f: (fun id -> id <> did)
        );
    }


  let subst var (oid, nid) =
    { derivations =
        List.map var.derivations ~f: (fun dv ->
          match dv with
          | Derivation.Terminal _ -> dv
          | Derivation.NonTerminal ids ->
            Derivation.NonTerminal (List.map ids ~f: (fun id ->
              if id = oid then
                nid
              else
                id
            ))
        );
    }

  let print id var =
    Id.print id ^
    " -> " ^
    String.concat ~sep: "|" (
      List.map ~f: Derivation.print var.derivations
    )
end

type t =
  { variables : Var.t Array.t;
  }

let copy cfg = ref
  { variables = Array.copy cfg.variables;
  }

let filter_vars cfg ~f =
  let (oids, dids) =
    List.init (Array.length !cfg.variables) ~f: ident |>
    List.partition_tf ~f in
  let nids =
    List.init (List.length oids) ~f: ident in
  let substs = List.zip_exn oids nids in
  cfg :=
    { variables =
        Array.of_list_map oids ~f: (fun oid ->
          let var = Array.get !cfg.variables oid in
          let var = List.fold dids ~init: var ~f: Var.elim in
          let var = List.fold substs ~init: var ~f: Var.subst in
          var
        );
    };
  not (List.is_empty dids)

(* Removes epsilons from non-terminal derivations,
   substitutes single-term non-terminal derivations,
   removes self or mutually recursive non-terminal derivations. *)
let optimize_growth cfg =
  let rec inline_epsilon () =
    let again = ref false in
    Util.Array.mapi_inplace !cfg.variables ~f: (fun id var ->
      { Var.derivations =
          List.concat_map var.Var.derivations ~f: (fun drv ->
            match drv with
            | Terminal _ -> [drv]
            | NonTerminal dids ->
              let ndids1 =
                List.filter dids ~f: (fun did ->
                  let dvar = Array.get !cfg.variables did in
                  List.is_empty dvar.derivations ||
                  not (List.for_all dvar.derivations ~f: Derivation.is_epsilon)
                ) in
              let ndids2 =
                List.filter dids ~f: (fun did ->
                  let dvar = Array.get !cfg.variables did in
                  List.is_empty dvar.derivations ||
                  not (List.exists dvar.derivations ~f: Derivation.is_epsilon)
                ) in
              if List.length ndids1 <> List.length dids then
                again := true;
              if List.length ndids1 = List.length ndids2 then
                [NonTerminal ndids1]
              else
                [NonTerminal ndids1; NonTerminal ndids2]
          )
      }
    );
    if !again then
      inline_epsilon ()
  and inline_unit () =
    let rec inline_drvs vids id var =
      List.concat_map var.Var.derivations ~f: (fun drv ->
        match drv with
        | Derivation.NonTerminal [uid] ->
          if Id.Set.mem vids uid then
            []
          else
            let uvar = Array.get !cfg.variables uid in
            let uvar = Var.subst uvar (uid, id) in
            inline_drvs (Id.Set.add vids uid) uid uvar
        | _ -> [drv]
      ) in
    Util.Array.mapi_inplace !cfg.variables ~f: (fun id var ->
      { Var.derivations = inline_drvs (Id.Set.singleton id) id var;
      }
    ) in
  inline_epsilon ();
  inline_unit ()

let optimize cfg =
  let dedup () =
    Util.Array.mapi_inplace !cfg.variables ~f: (fun id var ->
      { Var.derivations =
          List.dedup_and_sort var.Var.derivations ~compare;
      }
    ) in
  let filter_unused () =
    let _ = filter_vars cfg ~f: (fun id ->
      (id = 0) ||
      Array.existsi !cfg.variables ~f: (fun id2 var2 ->
        (id <> id2) &&
        List.exists var2.derivations ~f: (fun drv2 ->
          match drv2 with
          | Terminal _ -> false
          | NonTerminal rids ->
            List.exists rids ~f: (fun rid -> id = rid)
        )
      )
    ) in ()
  and filter_empty () =
    while filter_vars cfg ~f: (fun id ->
      (id = 0) || (
        let var = Array.get !cfg.variables id in
        not (List.is_empty var.derivations)
      )
    ) do () done
  and filter_dups () =
    while filter_vars cfg ~f: (fun id ->
      let var = Array.get !cfg.variables id in
      Array.for_alli !cfg.variables ~f: (fun id2 var2 ->
        if (id >= id2) || (var <> var2) then
          true
        else (
          Array.map_inplace !cfg.variables ~f: (fun var ->
            Var.subst var (id, id2)
          );
          false
        )
      )
    ) do () done in
  optimize_growth cfg;
  dedup ();
  filter_unused ();
  filter_empty ();
  filter_dups ()

(** Gets the terminals and variables at the given level level of all
    possible parse trees. *)
let term_levels cfg =
  Util.Stream.unfold [[Term.Variable 0]] ~f: (fun prev ->
    prev |>
    List.filter ~f: (List.exists ~f: (fun trm ->
      match trm with
      | Term.Letter _ -> false
      | Variable _ -> true
    )) |>
    List.concat_map ~f: (fun lvl ->
      lvl |>
      List.map ~f: (fun trm ->
        match trm with
        | Term.Letter _ -> [[trm]]
        | Term.Variable id ->
          let var = Array.get cfg.variables id in
          var.derivations |>
          List.map ~f: (fun drv ->
            match drv with
            | Derivation.Terminal l -> [Term.Letter l]
            | NonTerminal ids ->
              List.map ids ~f: (fun id -> Term.Variable id)
          ) (* e.g. [(a)|(bc)|(def)|(g)] *)
      ) |> (* e.g. [((a))((bd)|(be)|(bf))((cg)|(ch))] *)
      List.all |> (* e.g. [((a)(bd)(cg))|((a)(bd)(ch))|((a)(be(cg))|...] *)
      List.map ~f: List.concat (* e.g. [(abdcg)|(abdch)|(abecg)|(abech)|(abfcg)|(abfch)] *)
    ) |>
    List.map ~f: (List.filter ~f: (fun trm ->
      trm <> Term.Letter Letter.epsilon
    ))
  )

let word_levels cfg =
  Util.Stream.map (term_levels cfg) ~f: (fun lvl ->
    lvl |>
    List.filter ~f: (List.for_all ~f: (fun trm ->
      match trm with
      | Term.Letter _ -> true
      | Variable _ -> false
    )) |>
    List.map ~f: (fun lvl ->
      lvl |>
      List.map ~f: (fun trm ->
        match trm with
        | Term.Letter l -> l
        | Variable _ -> assert false
      ) |>
      Word.of_letters_no_epsilon
    )
  )

let all_words cfg =
  Util.Stream.concat (word_levels cfg)

let run_bool cfg w =
  let cfg = copy cfg
  and rem_lvls = ref (max 2 (Word.length w + 1))
  and res = ref false in
  optimize_growth cfg;
  let lvls = word_levels !cfg in
  ( try
      while not !res && !rem_lvls > 0 do
        let lvl = Stream.next lvls in
        if List.exists lvl ~f: (fun lw -> w = lw) then
          res := true;
        rem_lvls := !rem_lvls - 1
      done
    with
    | Stream.Failure -> ()
  );
  !res

let run cfg w =
  if run_bool cfg w then
    Run_result.Accept
  else
    Run_result.Reject 0

let parse vars =
  assert (not (List.is_empty vars));
  { variables = vars |> Util.Id.validate_order |> Array.of_list;
  }

let print cfg =
  cfg.variables |>
  Array.to_list |>
  List.mapi ~f: Var.print |>
  String.concat ~sep: "\n"
