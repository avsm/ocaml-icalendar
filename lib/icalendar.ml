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

  let rec parse_components_exn props acc =
    match props with
    | [] -> (List.rev acc,[])
    | {Property. name="END"; _ } :: _ ->
      (List.rev acc, props)
    | {Property. name="BEGIN"; data = kind; _ } :: tl ->
      let (components,rest) = parse_components_exn tl [] in
      begin match rest with
      | {Property. name="END"; data=end_kind; _ } :: tl ->
        if end_kind <> kind then failwith "mismatched END"
        else
          parse_components_exn tl (`Object { kind; components } :: acc)
      | _ -> failwith "No suitable END"
      end
    | line :: tl ->
      parse_components_exn tl (`Property line :: acc)

  let parse_exn props =
    let (components,rest) = parse_components_exn props [] in
    if not (List.is_empty rest) then failwith "Extra properties left over"
    else match components with
    | [`Object x] -> x
    | _ -> failwith "Expected exactly one object, found something else"

  let get_data t name =
    List.find_map t.components ~f:(function
    | `Object _ -> None
    | `Property l -> if l.Property.name = name then Some l.Property.data else None)

  let load_exn filename =
    In_channel.read_lines filename
    |! unfold_lines
    |! List.map ~f:Property.of_unfolded_line
    |! parse_exn
end

