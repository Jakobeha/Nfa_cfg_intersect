open Core

module Id = struct
  let validate_order ids =
    List.mapi ~f: (fun idx (id, a) -> assert (idx = id); a) ids
end

module List = struct
  let concat_fold lst ~init ~f =
    List.fold lst ~init: [init] ~f: (fun prevs a ->
      List.concat_map prevs ~f: (fun prev ->
        f prev a
      )
    )
end

module Array = struct
  let mapi_inplace ~f arr =
    Array.iteri arr ~f: (fun idx a ->
      Array.set arr idx (f idx a)
    )
end

module String = struct
  (* -1 if not found *)
  let findi ~f str =
    String.fold_until str ~init: 0 ~f: (fun pi c ->
      let i = pi + 1 in
      if f i c then
        Stop (i, c)
      else
        Continue i
    ) ~finish: (fun _ -> raise Caml.Not_found)
end

module Stream = struct
  let unfold init ~f =
    let idx = ref 0
    and itm = ref init in
    Stream.from (fun i ->
      assert (!idx <= i);
      while !idx < i do
        idx := !idx + 1;
        itm := f !itm
      done;
      Some !itm
    )

  let map stm ~f =
    let idx = ref 0 in
    Stream.from (fun i ->
      assert (!idx <= i);
      try
        while !idx < i do
          idx := !idx + 1;
          Stream.junk stm
        done;
        idx := !idx + 1;
        Some (f (Stream.next stm))
      with
      | Stream.Failure -> None
    )

  let rec concat stm =
    let get_fst () = Stream.next stm in
    let rst = concat stm in
    Stream.lcons get_fst rst
end
