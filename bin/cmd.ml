open Cmdliner

let cmd =
  ( Term.(const Action.intersect_nfa_cfg $ const ()),
    Term.info
      "nfa_cfg_intersect"
      ~doc: "intersect a Context-Free Grammar and a Nondeterministic Finite Automaton."
      ~man:
        [ `S Manpage.s_description;
          `I ("instructions", "
Run this command without any arguments.
Then, in $(b,stdin), input the CFG, followed by \"===\", followed by the NFA.
Make sure to signal end-of-file with \"^D\" when you're done inputting.
The program will calculate the intersection of the CFG and NFA, and print it to $(b,stdout).
");
         `S Manpage.s_examples;
         `P "
Intersect the CFG { (c*ac*)^n(c*bc*)^n | n >= 0 } ({ a^nb^n } ignoring the 'c's)
with the NFA { c?((a|b)c)* } (alternating 'a'/'b's and 'c's):
";
         `Pre "
S -> ASB|_
A -> a|CA|AC
B -> b|CB|BC
C -> c
===
S  -_-> A
   +_-> B
A@ -a-> B
   +b-> B
B@ -c-> A
         "
        ]
  )


let run () = Term.(exit @@ eval cmd)
