open Core

let validate_id_order id_as =
  List.mapi ~f: (fun idx (id, a) -> assert (idx = id); a) id_as
