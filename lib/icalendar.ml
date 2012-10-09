open Core.Std

(* merges lines with subsequent lines that start with a space, as per the
   RFC. *)
let unfold_lines lines =
  (* drop empty lines *)
  let lines = List.filter lines ~f:(fun s -> String.length s <> 0) in
  (* Group one line with the next when the second line starts with a space *)
  let grouped_lines = List.group lines ~break:(fun _ s -> s.[0] <> ' ') in
  (* Drop the first character from all but first line of each group, then
     concatenate.*)
  List.map grouped_lines ~f:(fun group ->
    String.concat
      (match group with
      | [] -> []
      | hd :: tl ->
        (hd :: List.map tl ~f:(fun s -> String.drop_prefix s 1))))

module Property = struct
  type t = { name: string;
             attrs: (string * string list) sexp_list;
             data: string;
           }
  with sexp

  let of_unfolded_line l =
    let (name_and_attrs,data) = String.lsplit2_exn l ~on:':' in
    match String.split ~on:';' name_and_attrs with
    | [] -> failwith "no name found"
    | name :: attrs ->
      let name = String.uppercase name in
      let attrs = List.map attrs ~f:(fun attr ->
        let (name,data) = String.lsplit2_exn ~on:'=' attr in
        (name,String.split ~on:',' data))
      in
      { name; attrs; data }
end

module Object = struct
  type t = { kind: string;
             components: [ `Object of t | `Property of Property.t ] sexp_list;
           }
  with sexp

  let rec parse_components props acc =
    match props with
    | [] -> (List.rev acc,[])
    | {Property. name="END"; _ } :: _ ->
      (List.rev acc, props)
    | {Property. name="BEGIN"; data = kind; _ } :: tl ->
      let (components,rest) = parse_components tl [] in
      begin match rest with
      | {Property. name="END"; data=end_kind; _ } :: tl ->
        if end_kind <> kind then failwith "mismatched END"
        else
          parse_components tl (`Object { kind; components } :: acc)
      | _ -> failwith "No suitable END"
      end
    | line :: tl ->
      parse_components tl (`Property line :: acc)

  let parse props =
    let (components,rest) = parse_components props [] in
    if not (List.is_empty rest) then failwith "Extra properties left over"
    else match components with
    | [`Object x] -> x
    | _ -> failwith "Expected exactly one object, found something else"

  let get t name =
    List.find_map t.components ~f:(function
    | `Object _ -> None
    | `Property l -> if l.Property.name = name then Some l.Property.data else None)

  let read filename =
    In_channel.read_lines filename
    |! unfold_lines
    |! List.map ~f:Property.of_unfolded_line
    |! parse
end

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
      Option.value_exn ~error (Object.get obj field)
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
      (<:sexp_of<Object.t>> (Object.read filename));
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
      Vacation.find_in_obj (Object.read filename)
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
