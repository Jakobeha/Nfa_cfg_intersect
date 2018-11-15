open Core

let validate_id_order id_as =
  List.mapi ~f: (fun idx (id, a) -> assert (idx = id); a) id_as

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
