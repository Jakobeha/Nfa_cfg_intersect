open Core

let validate_id_order id_as =
  List.mapi ~f: (fun idx (id, a) -> assert (idx = id); a) id_as

module Array = struct
  let mapi_inplace ~f arr =
    Array.iteri arr ~f: (fun idx a ->
      Array.set arr idx (f idx a)
    )
end
