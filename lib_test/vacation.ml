open Core.Std
open Icalendar

let () =
  Command.group
    ~summary:"Tool for managing vacation information"
    ["to-sexp", to_sexp]
  |! Command.run
