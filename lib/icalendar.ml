open Core.Std

(* merges lines with subsequent lines that start with a space, as per the RFC. *)
let unfold_lines lines =
  (* Group one line with the next when the second line starts with a space *)
  let grouped_lines = List.group lines ~break:(fun _ s -> not (String.is_prefix ~prefix:" " s)) in
  (* Drop the first character from all but the first line of each group, then
     concatenate.*)
  List.map grouped_lines ~f:(fun group ->
    String.concat
      (match group with
      | [] -> []
      | hd :: tl ->
        (hd :: List.map tl ~f:(fun s -> String.drop_prefix s 1))))

module Line = struct
  type t = { key: string;
             attrs: (string * string) list;
             data: string;
           }
  with sexp

  let of_string l =
    let (key_and_attrs,data) = String.lsplit2_exn l ~on:':' in
    match String.split ~on:';' key_and_attrs with
    | [] -> failwith "no key found"
    | key::attrs ->
      let attrs = List.map attrs ~f:(String.lsplit2_exn ~on:'=') in
      { key; attrs; data }
end

module Block = struct
  type t = { kind: string;
             contents: [ `Block of t | `Line of Line.t ] sexp_list;
           }
  with sexp

  let rec parse_contents lines acc =
    match lines with
    | [] -> (List.rev acc,[])
    | {Line. key="END"; _ } :: _ ->
      (List.rev acc, lines)
    | {Line. key="BEGIN"; data=kind; _ } :: tl ->
      let (contents,rest) = parse_contents tl [] in
      begin match rest with
      | {Line. key="END"; data=end_kind; _ } :: tl ->
        if end_kind <> kind then failwith "mismatched END"
        else
          parse_contents tl (`Block { kind; contents } :: acc)
      | _ -> failwith "No suitable END"
      end
    | line :: tl ->
      parse_contents tl (`Line line :: acc)

  let parse lines =
    let (contents,rest) = parse_contents lines [] in
    if not (List.is_empty rest) then failwith "Extra lines left over"
    else match contents with
    | [`Block x] -> x
    | _ -> failwith "Expected exactly one block, found something else"

  let get t key =
    List.find_map t.contents ~f:(function
      | `Block _ -> None
      | `Line l -> if l.Line.key = key then Some l.Line.data else None)
end

module Vacation = struct
  type t = { date: Date.t;
             summary: string;
           }
  with sexp

  let rec of_vevent block =
    let date_of_string s =
      (* Drop times, in this case, we only want to consider dates *)
      match String.lsplit2 s ~on:'T' with
      | None       -> Date.of_string s
      | Some (s,_) -> Date.of_string s
    in
    let get field =
      let error = Error.tag (Error.create field block <:sexp_of<Block.t>>)
        "missing field"
      in
      Option.value_exn ~error (Block.get block field)
    in
    let min = date_of_string (get "DTSTART") in
    let max = date_of_string (get "DTEND")   in
    let summary = String.lowercase (get "SUMMARY") in
    let dates = Date.weekdays_between ~min ~max in
    List.map dates ~f:(fun date -> { date; summary })

  let rec find_in_block b =
    if b.Block.kind = "VEVENT" then of_vevent b
    else
      List.bind b.Block.contents (function
        | `Line _ -> []
        | `Block b -> find_in_block b)
end

let string_match x ~within =
  let re = Re.str x |! Re.compile in
  Re.execp re within

let to_sexp = Command.basic
  ~summary:"Parse ical file and dump sexp of vacations"
  Command.Spec.(
    empty
    +> anon ("<icalendar file>"%:string)
    +> flag "-year" (optional int) ~doc:" Just this year"
    +> flag "-who" (optional string) ~doc:" Just this user"
    +> flag "-vacation" no_arg ~doc:" Just entries with word vacation"
  )
  (fun filename year who vacation_only ->
    let vacations =
      In_channel.read_lines filename
      |! unfold_lines
      |! List.map ~f:Line.of_string
      |! Block.parse
      |! Vacation.find_in_block
    in
    let vacations =
      let keep = Fn.const true in
      vacations
      |! List.filter ~f:(match year with
        | None -> keep
        | Some year -> (fun v -> Date.year v.Vacation.date = year))
      |! List.filter ~f:(match who with
        | None -> keep
        | Some who -> (fun v -> string_match who ~within:v.Vacation.summary))
      |! List.filter ~f:(if not vacation_only then keep else
          (fun v -> string_match "vacation" ~within:v.Vacation.summary))
      |! List.sort ~cmp:(fun x y ->
        Date.compare x.Vacation.date y.Vacation.date)
    in
    Sexp.output_hum stdout
      (<:sexp_of<Vacation.t list>> vacations);
    printf "\n"
  )
