open Cmdliner

(* let src_workspace_arg =
  Arg.
    ( value &
      pos 0 dir (Sys.getcwd ()) &
      info
        []
        ~docv: "SRC"
        ~doc: "The path to the a directory containing source files. Defaults to the working directory"
    )

let src_prog_arg =
  Arg.
    ( required &
      pos 0 (some non_dir_file) None &
      info
        []
        ~docv: "SRC"
        ~doc: "The path to the program's source code"
    )

let eval_cmd =
  let input = src_prog_arg in
  ( Term.(const Action.evaluate $ input),
    Term.info
      "eval"
      ~doc: "interpret the input program, print its result."
      ~man:
        [ `S Manpage.s_description;
          `P "
Given source code for a descript program,
this will parse the program,
reduce its query (value before the '?'),
and print out the result.
For example, given a path to:";
          `Pre "
  Foo[]: Bar[];
  ---
  Foo[]?
";
          `P "This will output:";
          `Pre "
  Bar[]";
        ]
  )

let compile_cmd =
  let input = src_prog_arg
  and output =
  Arg.
    ( value &
      opt (some string) None &
      info
        ["out"]
        ~docv: "OUT"
        ~doc: "
The path to the compiled output, without the extension -
e.g. if 'foo/bar/baz' is specified and the output is javascript,
it will be written to 'foo/bar/baz.js'.
If not specified, this is $(b,SRC) without the '.dscr',
e.g. the file 'foo/bar/baz.dscr' will compile to 'foo/bar/baz.js'"
    ) in
  ( Term.(const Action.compile $ input $ output),
    Term.info
      "compile"
      ~doc: "interpret the input program, write its result."
      ~man:
        [ `S Manpage.s_description;
          `P "
Given source code for a descript program,
this will parse the program,
then reduce its query (value before the '?').
The program's query must reduce to a code block
(matches 'Code[lang: <String; content: <String]').
The text in 'content' will be written to the file
$(b,OUT).<lang>.
For example, given the file:";
          `Pre "
  Foo[]: Code[lang: \"js\"; content: \"console.log(\\\"Working\\\");\"];
  ---
  Foo[]?
";
          `P "And '--out \"out/foo\", this will write to \"out/foo.js\":";
          `Pre "
  console.log(\"Working\");"
        ]
  )

let refactor_cmd =
  let input = src_prog_arg
  and phase_in =
    Arg.
      ( value &
        opt (some non_dir_file) None &
        info
          ["using"]
          ~docv: "PHASE"
          ~doc: "
The path to the phase's source code.
If not specified, the phase will be read from $(b,STDIN),
and it will stop being read when '---' is encountered."
      )
  and output =
    Arg.
      ( value &
        opt (some string) None &
        info
          ["out"]
          ~docv: "OUT"
          ~doc: "
The path to where the new source code will be written.
Defaults to $(b,SRC), so that the original source code gets overwritten."
      ) in
  ( Term.(const Action.refactor $ input $ phase_in $ output),
    Term.info
      "refactor"
      ~doc: "macro-reduce all values within a program's source."
      ~man:
        [ `S Manpage.s_description;
          `P "
Given source code for a descript program,
source code for a single phase, and an output,
this will parse the program and phase,
macro-reduce the program using the phase,
and write the new program to the output.
For example, given a path to the program:";
          `Pre "
  Foo[a: <]: Qux[q: >a];
  ---
  Qux[q: 4]?
";
          `P "and the phase:";
          `Pre "
  Foo[a: <]: Bar[b: >a];
  Qux[q: <Integer]: >q;
";
          `P "This will output:";
          `Pre "
  Bar[b: <Integer]: >b;
  Bar[b: <]: Qux[q: >b];
  ---
  4?";
        ]
  )

let start_env_cmd =
  let input = src_workspace_arg
  and no_teardown =
    Arg.
      ( value &
        flag &
        info
          ["no-teardown"]
          ~docv: "NO-TEARDOWN"
          ~doc: "
If absent, will delete all $(i,.refactor.dscr) files when the environment
stops (via Ctrl-C)") in
  ( Term.(const Action.start_env $ input $ no_teardown),
    Term.info
      "start-env"
      ~doc: "start up an environment to help you code."
      ~man:
        [ `S Manpage.s_description;
          `I ("how to use", "
First, open up an IDE for editing source code,
preferrably with multiple panes, such as VSCode.
Next, run $(b,start-env <workspace>),
where <workspace> is a folder containing your descript files.
Now, whenever you edit a $(i,.dscr) file in the workspace, and it parses,
the output will be printed to the terminal.
Additionally, each $(i,.dscr) file should have a corresponding
$(i,.refactor.dscr) file. Whenever you open the $(i,.dscr) file,
you should open the $(i,.refactor.dscr) file in the other pane.
You can use it to refactor - if you enter reducers into the
$(i,.refactor.dscr) file, then enter a phase separator, the reducers
will be applied to the $(i,.dscr) file and removed from the
$(i,.refactor.dscr). When you're finished editing,
use Ctrl-C to stop the environment");
          `I ("specifics", "
For each $(i,.dscr) file in the workspace <source>.dscr, this will do multiple things:
- Whenever the file is modified, it'll be parsed and interpreted.
  If it can't be parsed or interpreted, the error will be printed.
  If it can be parsed and interpreted, its result (the reduced query) will
  be printed (as if running \"descript eval\"). (In the future, if the result
  is a code block, it'll be compiled instead. Right now code blocks don't exist.)
- This will first create a corresponding file <source>.refactor.dscr.
  Then, it will watch <source>.dscr and <source>.refactor.dscr for changes.
  When both files parse and a complete phase followed by a separator is
  entered into <source>.refactor.dscr, the phase and separator will be deleted,
  and they'll be used to macro-reduce from <source>.dscr.");
        ]
  )

let cmds = [intersect_cmd, complement_cmd, minimize_cmd] *)

let cmds = []

let default_cmd =
  Term.(ret (const (`Help (`Plain, None)))),
  Term.info
    "nfa_cfg_intersect"
    ~version:"v0.3.2"
    ~doc: "command-line interface for nfa_cfg_intersect"
    ~sdocs: Manpage.s_common_options

let run () = Term.(exit @@ eval_choice default_cmd cmds)
