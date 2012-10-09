open Core.Std
open Icalendar

module Vacation = struct
  type t = { date: Date.t;
             summary: string;
           }
  with sexp

  let of_vevent obj =
    let date_of_string s =
      (* Drop times, in this case, we only want to consider dates *)
      match String.lsplit2 s ~on:'T' with
      | None       -> Date.of_string s
      | Some (s,_) -> Date.of_string s
    in
    let get field =
      let error = Error.tag (Error.create field obj <:sexp_of<Object.t>>)
        "missing field"
      in
      Option.value_exn ~error (Object.get_data obj field)
    in
    let min = date_of_string (get "DTSTART") in
    let max = date_of_string (get "DTEND")   in
    let summary = String.lowercase (get "SUMMARY") in
    let dates = Date.weekdays_between ~min ~max in
    List.map dates ~f:(fun date -> { date; summary })

  let rec find_in_obj o =
    if o.Object.kind = "VEVENT" then of_vevent o
    else
      List.bind o.Object.components (function
        | `Property _ -> []
        | `Object o -> find_in_obj o)
end

let string_match x ~within =
  let re = Re.str x |! Re.compile in
  Re.execp re within

let full_parse = Command.basic
  ~summary:"Dump the parsed output of the ical data, with full information"
  Command.Spec.(
    empty
    +> anon ("<icalendar file>" %: string)
  )
  (fun filename ->
    Sexp.output_hum stdout
      (<:sexp_of<Object.t>> (Object.load_exn filename));
    printf "\n"
  )
;;

let to_sexp = Command.basic
  ~summary:"Parse ical file and dump sexp of vacations"
  Command.Spec.(
    empty
    +> anon ("<icalendar file>" %: string)
    +> flag "-year" (optional int) ~doc:" Just this year"
    +> flag "-who" (optional string) ~doc:" Just this user"
    +> flag "-vacation" no_arg ~doc:" Just entries with word vacation"
  )
  (fun filename year who vacation_only ->
    let keep_all = Fn.const true in (* to use in filters that keep everything *)
    let vacations =
      Vacation.find_in_obj (Object.load_exn filename)
      (* Filter the list of vacations, according to the command-line arguments *)
      |! List.filter ~f:(match year with
        | None -> keep_all
        | Some year -> (fun v -> Date.year v.Vacation.date = year))
      |! List.filter ~f:(match who with
        | None -> keep_all
        | Some who -> (fun v -> string_match who ~within:v.Vacation.summary))
      |! List.filter ~f:(
        if not vacation_only then keep_all
        else (fun v -> string_match "vacation" ~within:v.Vacation.summary))
      |! List.sort ~cmp:(fun x y ->
        Date.compare x.Vacation.date y.Vacation.date)
    in
    Sexp.output_hum stdout
      (<:sexp_of<Vacation.t list>> vacations);
    printf "\n"
  )

let () =
  Command.group
    ~summary:"Tool for managing vacation information"
    [ "to-sexp", to_sexp
    ; "full-parse", full_parse
    ]
  |! Command.run

