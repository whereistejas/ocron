open Core
open Sexplib.Std
module Time = Time_float_unix
module Span = Time.Span
module Zone = Time.Zone

module U = struct
  (** Returns current time with minutes and seconds set to zero. *)
  let truncate_hr (time : Time.t) : Time.t =
    let zone = Lazy.force Zone.local in
    let date = Time.to_date ~zone time in
    let ofday_parts = Time.to_ofday ~zone time |> Time.Ofday.to_parts in
    let ofday =
      Time.Ofday.create ~hr:ofday_parts.hr ~min:0 ~sec:0 ~ms:0 ~us:0 ~ns:0 ()
    in
    Time.of_date_ofday ~zone date ofday

  (** Returns current time with hours, minutes and seconds set to zero. *)
  let truncate_day (time : Time.t) : Time.t =
    let zone = Lazy.force Zone.local in
    let date = Time.to_date ~zone time in
    Time.of_date_ofday ~zone date Time.Ofday.start_of_day

  (** Returns current time with days, hours, minutes and seconds set to zero. *)
  let truncate_month (time : Time.t) : Time.t =
    let zone = Lazy.force Zone.local in
    let date = Time.to_date ~zone time in
    let date = Date.create_exn ~y:(Date.year date) ~m:(Date.month date) ~d:0 in
    Time.of_date_ofday ~zone date Time.Ofday.start_of_day
end

module Types = struct
  type 'span element =
    | Single of 'span
    | Range of {
        max : 'span;
        min : 'span;
      }
  [@@deriving sexp, compare]

  type 'span value =
    | All
    | Value of 'span element
    | List of 'span element list
  [@@deriving sexp, compare]

  type schedule = {
    (* Valid values: [0,59] *)
    minute : Span.t value;
    (* Valid values: [0,23] *)
    hour : Span.t value;
    (* Valid values: [1,31] *)
    day_of_month : Span.t value;
    (* Valid values: [1,12] *)
    month : Month.t value;
    (* Valid values: [0,6] with 0=Sunday *)
    day_of_week : Day_of_week.t value;
  }
  [@@deriving sexp, compare]

  type intermediate_schedule = {
    minute : Span.t list;
    hour : Span.t list;
    day_of_month : Span.t list;
    month : Month.t list;
    day_of_week : Day_of_week.t list;
    (* TODO: Collapse above day and month fields into [dates] *)
    dates : Date.t list;
  }
  [@@deriving sexp, compare]

  let max_minute : int = 59
  let max_hour : int = 23
  let max_day_of_month : int = 31
  let max_month : int = 12
  let max_day_of_week : int = 6
  let min_minute : int = 0
  let min_hour : int = 0
  let min_day_of_month : int = 1
  let min_month : int = 1
  let min_day_of_week : int = 0
end

module Parse = struct
  open Types

  (** In the POSIX locale, the user or application shall ensure that a crontab *
      entry is a text file consisting of lines of six fields each. The fields *
      shall be separated by <blank>s. * * The first five fields shall be integer
      patterns that specify: *)
  let tokenise value : string * string * string * string * string =
    let fields =
      String.split_on_chars value ~on:[ ' '; '\t' ]
      |> List.filter ~f:(fun field -> not (String.is_empty field))
      |> List.map ~f:String.strip
    in
    match fields with
    | [ minute_str; hour_str; day_of_month_str; month_str; day_of_week_str ] ->
        (minute_str, hour_str, day_of_month_str, month_str, day_of_week_str)
    | _ -> raise (Invalid_argument "Invalid cron expression")

  (** An element shall be either a number or two numbers separated by * a hyphen
      (meaning an inclusive range). *)
  let parse_element (value : string) (parse : int -> 'time) : 'time element =
    match value with
    | value when String.contains value '-' ->
        String.lsplit2 ~on:'-' value
        |> Option.map ~f:(fun (min, max) ->
               let min = int_of_string min in
               let max = int_of_string max in
               Range { max = parse max; min = parse min })
        |> Option.value_exn
    | value ->
        let value = int_of_string value in
        Single (parse value)

  (** Each of these patterns can be either an asterisk (meaning all valid *
      values), an element, or a list of elements separated by commas. *)
  let parse_value (value : string) (parse : int -> 'time) : 'time value =
    match value with
    | "*" -> All
    | value when String.contains value ',' ->
        let elements =
          String.split value ~on:','
          |> List.map ~f:(fun value -> parse_element value parse)
        in
        List elements
    | value -> Value (parse_element value parse)

  let parse (value : string) : schedule =
    let minute_str, hour_str, day_str, month_str, dow_str = tokenise value in
    let minute = parse_value minute_str (fun min -> Time.Span.create ~min ()) in
    let hour = parse_value hour_str (fun hr -> Time.Span.create ~hr ()) in
    let day = parse_value day_str (fun day -> Time.Span.create ~day ()) in
    let month = parse_value month_str (fun month -> Month.of_int_exn month) in
    let day_of_week =
      parse_value dow_str (fun day -> Day_of_week.of_int_exn day)
    in
    { minute; hour; day_of_month = day; month; day_of_week }
end

module ToTime = struct
  open Types

  (* The specification of days can be made by two fields (day
   * of the month and day of the week). If month, day of month, and day
   * of week are all <asterisk> characters, every day shall be matched.
   * If either the month or day of month is specified as an element or
   * list, but the day of week is an <asterisk>, the month and day of
   * month fields shall specify the days that match. If both month and
   * day of month are specified as an <asterisk>, but day of week is an
   * element or list, then only the specified days of the week match.
   * Finally, if either the month or day of month is specified as an
   * element or list, and the day of week is also specified as an
   * element or list, then any day matching either the month and day of
   * month, or the day of week, shall be matched. *)

  let element_to_span ~(to_span : int -> Span.t) ~(to_int : Span.t -> int)
      (element : 'time element) : Span.t list =
    match element with
    | Single el -> [ el ]
    | Range { min; max } ->
        List.range (to_int min) (to_int max) |> List.map ~f:to_span

  let value_to_span ~(min : int) ~(max : int) ~(to_span : int -> Span.t)
      ~(to_int : Span.t -> int) (value : 'time value) : Span.t list =
    match value with
    | All -> List.range min max |> List.map ~f:to_span
    | Value el -> element_to_span ~to_span ~to_int el
    | List els -> List.concat_map els ~f:(element_to_span ~to_span ~to_int)

  let element_to_month (element : 'time element) : Month.t list =
    match element with
    | Single el -> [ el ]
    | Range { min; max } ->
        List.filter Month.all ~f:(fun month ->
            Month.( >= ) min month && Month.( >= ) month max)

  let value_to_month (value : 'time value) : Month.t list =
    match value with
    | All -> Month.all
    | Value el -> element_to_month el
    | List els -> List.concat_map els ~f:(fun el -> element_to_month el)

  let schedule_to_time ({ minute; hour; day_of_month; month; _ } : schedule) :
      intermediate_schedule =
    let minute =
      value_to_span ~min:min_minute ~max:max_minute
        ~to_span:(fun int -> Span.create ~min:int ())
        ~to_int:(fun span -> Span.to_hr span |> int_of_float)
        minute
    in
    let hour =
      value_to_span ~min:min_hour ~max:max_hour
        ~to_span:(fun int -> Span.create ~hr:int ())
        ~to_int:(fun span -> Span.to_min span |> int_of_float)
        hour
    in
    let day_of_month =
      value_to_span ~min:min_day_of_month ~max:max_day_of_month
        ~to_span:(fun int -> Span.create ~day:int ())
        ~to_int:(fun span -> Span.to_day span |> int_of_float)
        day_of_month
    in
    let month = value_to_month month in
    { minute; hour; day_of_month; month; day_of_week = []; dates = [] }
end

(** Returns next occurrence after NOW according to the schedule. *)
let next ?start_from (schedule : Types.schedule) : Time.t =
  let open Types in
  let open ToTime in
  let print_debug name sexp_of value =
    Printf.printf "%s: %s\n" name (Sexp.to_string_hum (sexp_of value))
  in
  let { minute; hour; day_of_month; month; _ } = schedule_to_time schedule in
  let () = print_debug "minute" [%sexp_of: Span.t list] minute in
  let () = print_debug "hour" [%sexp_of: Span.t list] hour in
  let () = print_debug "day_of_month" [%sexp_of: Span.t list] day_of_month in
  let () = print_debug "month" [%sexp_of: Month.t list] month in

  let zone = Lazy.force Zone.local in
  let rec next ~(start_date : Date.t) ~check_date ~spans : Time.t =
    let next_date =
      List.find_map spans ~f:(fun span ->
          let span = Time.Ofday.of_span_since_start_of_day_exn span in
          let next_date = Time.of_date_ofday ~zone start_date span in
          if Time.compare next_date check_date >= 0 then Some next_date
          else None)
    in
    Option.value_or_thunk next_date ~default:(fun () ->
        let start_date = Date.add_days start_date 1 in
        next ~start_date ~check_date ~spans)
  in

  let spans =
    let hours_and_minutes = List.cartesian_product hour minute in
    List.map hours_and_minutes ~f:(fun (hr, min) -> Span.( + ) hr min)
  in
  let today = Option.value start_from ~default:(Time.now ()) in
  let date = Time.to_date ~zone today in
  let dates : Date.t list =
    let months_and_days = List.cartesian_product month day_of_month in
    List.map months_and_days ~f:(fun (month, day) ->
        (* TODO: Check if days are within the limit of the month we are setting *)
        let new_date =
          Date.create_exn ~y:(Date.year date) ~m:month
            ~d:(Span.to_day day |> int_of_float)
        in
        new_date)
  in
  let () = print_debug "dates" [%sexp_of: Date.t list] dates in
  next ~start_date:date ~check_date:today ~spans
