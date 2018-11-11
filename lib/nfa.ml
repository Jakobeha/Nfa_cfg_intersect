open Core

module Arrow = struct
  type t =
    { consume : Letter.t;
    }

  let print arr =
    Letter.print arr.consume
end

include Automaton.Make(Arrow)
