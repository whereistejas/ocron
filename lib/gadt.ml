(* Phantom types for schema kinds *)
type object_t
type array_t
type string_t
type number_t
type boolean_t
type any_t

(* GADT encoding schema structure *)
type _ schema =
  | Object : {
      properties : (string * any_t schema) list;
      required : string list;
      additional_properties : bool;
    }
      -> object_t schema
  | Array : {
      items : any_t schema;
      min_items : int option;
      max_items : int option;
      unique_items : bool;
    }
      -> array_t schema
  | String : {
      pattern : string option;
      min_length : int option;
      max_length : int option;
      enum : string list option;
    }
      -> string_t schema
  | Number : {
      minimum : float option;
      maximum : float option;
      exclusive_minimum : bool;
      multiple_of : float option;
    }
      -> number_t schema
  | Boolean : boolean_t schema
  (* Existential wrapper for heterogeneous collections *)
  | Any : 'a schema -> any_t schema

(* Smart constructors *)
let obj ~properties ?(required = []) ?(additional_properties = true) () =
  Object { properties; required; additional_properties }

let array ~items ?min_items ?max_items ?(unique_items = false) () =
  Array { items; min_items; max_items; unique_items }

let string ?pattern ?min_length ?max_length ?enum () =
  String { pattern; min_length; max_length; enum }

let number ?minimum ?maximum ?(exclusive_minimum = false) ?multiple_of () =
  Number { minimum; maximum; exclusive_minimum; multiple_of }

let boolean = Boolean

(* Type-safe validation functions *)
let rec validate : type a. a schema -> Yojson.Basic.t -> (unit, string) result =
 fun schema json ->
  match (schema, json) with
  | Object { properties; required; _ }, `Assoc fields ->
      (* Validate required fields present *)
      let missing =
        List.filter (fun req -> not (List.mem_assoc req fields)) required
      in
      if missing <> [] then
        Error
          (Printf.sprintf "Missing required fields: %s"
             (String.concat ", " missing))
      else
        (* Validate each property *)
        let validate_prop (name, Any prop_schema) =
          match List.assoc_opt name fields with
          | None -> Ok ()
          | Some value -> Result.map (fun _ -> ()) (validate prop_schema value)
        in
        List.fold_left
          (fun acc prop -> Result.bind acc (fun () -> validate_prop prop))
          (Ok ()) properties
  | Array { items; min_items; max_items; unique_items = _ }, `List values ->
      let len = List.length values in
      let check_min =
        match min_items with
        | Some min when len < min ->
            Error (Printf.sprintf "Array too short: %d < %d" len min)
        | _ -> Ok ()
      in
      let check_max =
        Result.bind check_min (fun () ->
            match max_items with
            | Some max when len > max ->
                Error (Printf.sprintf "Array too long: %d > %d" len max)
            | _ -> Ok ())
      in
      Result.bind check_max (fun () ->
          (* Validate each item *)
          List.fold_left
            (fun acc value ->
              Result.bind acc (fun () ->
                  Result.map (fun _ -> ()) (validate items value)))
            (Ok ()) values)
  | String { pattern = _; min_length; max_length; enum }, `String s ->
      let len = String.length s in
      let check_min =
        match min_length with
        | Some min when len < min ->
            Error (Printf.sprintf "String too short: %d < %d" len min)
        | _ -> Ok ()
      in
      let check_max =
        Result.bind check_min (fun () ->
            match max_length with
            | Some max when len > max ->
                Error (Printf.sprintf "String too long: %d > %d" len max)
            | _ -> Ok ())
      in
      Result.bind check_max (fun () ->
          match enum with
          | Some allowed when not (List.mem s allowed) ->
              Error "String not in enum"
          | _ -> Ok ())
  | Number { minimum; maximum; exclusive_minimum; multiple_of = _ }, `Int i ->
      let f = float_of_int i in
      let check_min =
        match minimum with
        | Some min when (exclusive_minimum && f <= min) || f < min ->
            Error "Number below minimum"
        | _ -> Ok ()
      in
      Result.bind check_min (fun () ->
          match maximum with
          | Some max when f > max -> Error "Number above maximum"
          | _ -> Ok ())
  | Number { minimum; maximum; exclusive_minimum; multiple_of = _ }, `Float f ->
      let check_min =
        match minimum with
        | Some min when (exclusive_minimum && f <= min) || f < min ->
            Error "Number below minimum"
        | _ -> Ok ()
      in
      Result.bind check_min (fun () ->
          match maximum with
          | Some max when f > max -> Error "Number above maximum"
          | _ -> Ok ())
  | Boolean, `Bool _ -> Ok ()
  | Any inner, json -> validate inner json
  | _ -> Error "Type mismatch"

(* Example usage *)
let user_schema =
  obj
    ~properties:
      [
        ("name", Any (string ~min_length:1 ()));
        ("age", Any (number ~minimum:0.0 ()));
        ("email", Any (string ~pattern:".*@.*" ()));
        ("tags", Any (array ~items:(Any (string ())) ()));
      ]
    ~required:[ "name"; "age" ] ()

(* Compile-time guarantee: can't add `items` to object schema *)
(* This won't compile:
   let bad = Object { properties = []; required = []; items = ... }
*)

(* Type-indexed extraction *)
let get_string : string_t schema -> Yojson.Basic.t -> string option =
 fun (String _) json -> match json with `String s -> Some s | _ -> None

let get_number : number_t schema -> Yojson.Basic.t -> float option =
 fun (Number _) json ->
  match json with
  | `Int i -> Some (float_of_int i)
  | `Float f -> Some f
  | _ -> None
