(** A module for parsing iCalendar and vCard style files *)

open Core.Std

module Property : sig
  (** The type of a property.  This representation of the property does not
      reflect any information about the nature of different properties and the
      types of their associated data. *)
  type t = { name: string;
             attrs: (string * string list) list;
             data: string;
           }
  with sexp
end

module Object : sig
  (** An [Object.t] has a kind, as well as a list of components, which may
      themselves by other objects or simply properties.  *)
  type t =
    { kind: string;
      components: [ `Object of t | `Property of Property.t ] sexp_list;
    }
  with sexp

  (** Lookup the data element of a property contained in the object in
      question *)
  val get_data : t -> string -> string option

  (** [load_exn filename] loads an object from the file [filename] *)
  val load_exn : string -> t
end

