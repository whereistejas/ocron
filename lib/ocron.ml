open Core
open Sexplib.Std
module Time = Time_float_unix
module Span = Time.Span
module Zone = Time.Zone

module U = struct
  let truncate_hr (time : Time.t) : Time.t =
    let zone = Lazy.force Zone.local in
    let date = Time.to_date ~zone time in
    let ofday = Time.to_ofday ~zone time in
    let parts = Time.Ofday.to_parts ofday in
    let ofday = Time.Ofday.create ~hr:parts.hr () in
    Time.of_date_ofday ~zone date ofday

  let truncate_day (time : Time.t) : Time.t =
    let zone = Lazy.force Zone.local in
    let date = Time.to_date ~zone time in
    let date = Date.add_days date 1 in
    let ofday = Time.to_ofday ~zone time in
    Time.of_date_ofday ~zone date ofday
end

(* The specification of days can be made by two fields (day of the month and
 * day of the week). If month, day of month, and day of week are all asterisks,
 * every day shall be matched. If either the month or day of month is specified
 * as an element or list, but the day of week is an asterisk, the month and day
 * of month fields shall specify the days that match. If both month and day of
 * month are specified as an asterisk, but day of week is an element or list,
 * then only the specified days of the week match.
 *
 * Finally, if either the month or day of month is specified as an element or
 * list, and the day of week is also specified as an element or list, then any
 * day matching either the month and day of month, or the day of week, shall
 * be matched. *)

(* An element shall be either a number or two numbers separated by
 * a hyphen (meaning an inclusive range). *)
type 'time element =
  | Single of 'time
  | Range of {
      max : 'time;
      min : 'time;
    }
[@@deriving sexp, compare]

(* Each of these patterns can be either an asterisk (meaning
 * all valid values), an element, or a list of elements separated
 * by commas. *)
and 'time value =
  | All
  | Value of 'time element
  | List of 'time element list
[@@deriving sexp, compare]

(* In the POSIX locale, the user or application shall ensure that a crontab
 * entry is a text file consisting of lines of six fields each. The fields
 * shall be separated by <blank>s. The first five fields shall be integer
 * patterns that specify the following:
 * 1. Minute [0,59]
 * 2. Hour [0,23]
 * 3. Day of the month [1,31]
 * 4. Month of the year [1,12]
 * 5. Day of the week ([0,6] with 0=Sunday) *)
and schedule = {
  minute : Span.t value;
  hour : Span.t value;
  day_of_month : Span.t value;
  month : Month.t value;
  day_of_week : Day_of_week.t value;
}
[@@deriving sexp, compare]

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

let parse_schedule (value : string) : schedule =
  let minute_str, hour_str, day_str, month_str, dow_str = tokenise value in
  let minute = parse_value minute_str (fun min -> Time.Span.create ~min ()) in
  let hour = parse_value hour_str (fun hr -> Time.Span.create ~hr ()) in
  let day = parse_value day_str (fun day -> Time.Span.create ~day ()) in
  let month = parse_value month_str (fun month -> Month.of_int_exn month) in
  let day_of_week =
    parse_value dow_str (fun day -> Day_of_week.of_int_exn day)
  in
  { minute; hour; day_of_month = day; month; day_of_week }

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

(* Returns next occurrence after UNIX_EPOCH according to the schedule. *)
let next (schedule : schedule) ~(start_from : Time.t) : Time.t list =
  let { minute; hour; _ } = schedule in
  let next_hour = Time.add (U.truncate_hr start_from) (Span.create ~hr:1 ()) in
  let _minutes =
    value_to_span ~min:0 ~max:59
      ~to_span:(fun int -> Span.create ~min:int ())
      ~to_int:(fun span -> Span.to_min span |> int_of_float)
      minute
    |> List.map ~f:(fun minute -> Time.add next_hour minute)
  in
  let next_day = Time.add (U.truncate_day start_from) (Span.create ~day:1 ()) in
  let _hours =
    value_to_span ~min:0 ~max:23
      ~to_span:(fun int -> Span.create ~hr:int ())
      ~to_int:(fun span -> Span.to_hr span |> int_of_float)
      hour
    |> List.map ~f:(fun hour -> Time.add next_day hour)
  in
  []
