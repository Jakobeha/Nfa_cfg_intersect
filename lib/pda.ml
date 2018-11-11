open Core

module Arrow = struct
  type t =
    { pop : Letter.t;
      push : Letter.t;
      consume : Letter.t;
    }

  let print arr =
    "[" ^
    Letter.print arr.consume ^
    "," ^
    Letter.print arr.pop ^
    "->" ^
    Letter.print arr.push ^
    "]"
end

include Automaton.Make(Arrow)
