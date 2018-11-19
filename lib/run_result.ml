open Core

type t =
  | Accept
  | Reject of Int.t

let compare a b =
  match (a, b) with
  | (Accept, Accept) -> 0
  | (Accept, Reject _) -> -1
  | (Reject _, Accept) -> 1
  | (Reject ai, Reject bi) -> bi - ai
