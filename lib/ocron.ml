open Core
open Sexplib.Std
module Time = Time_float_unix
module Span = Time.Span
module Zone = Time.Zone

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
let min_minute : int = 0
let max_hour : int = 23
let min_hour : int = 0
let max_day_of_month : int = 31
let min_day_of_month : int = 1

module U = struct
  let print_debug name sexp_of value =
    Printf.printf "%s: %s\n" name (Sexp.to_string_hum (sexp_of value))

  let debug_schedule
      ({ minute; hour; day_of_month; month; _ } : intermediate_schedule) =
    let () = print_debug "minute" [%sexp_of: Span.t list] minute in
    let () = print_debug "hour" [%sexp_of: Span.t list] hour in
    let () = print_debug "day_of_month" [%sexp_of: Span.t list] day_of_month in
    let () = print_debug "month" [%sexp_of: Month.t list] month in
    ()
end

module Parse = struct
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

  (** Convert cron syntax to types that correspond to the time units *)
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

module Schedule = struct
  let calculate_dates ?(skip_until : Month.t option) year schedule =
    let months =
      Option.value_map skip_until
        ~f:(fun skip ->
          List.filter schedule.month ~f:(fun month -> Month.( >= ) month skip))
        ~default:schedule.month
    in

    List.cartesian_product months schedule.day_of_month
    |> List.filter_map ~f:(fun (month, day) ->
           let day = Span.to_day day |> int_of_float in
           (* NOTE: Skipping invalid days (like 29 Feb in non-leap years) *)
           try Some (Date.create_exn ~y:year ~m:month ~d:day) with _ -> None)

  let merge_dates_and_times zone dates times =
    List.cartesian_product dates times
    |> List.map ~f:(fun (date, time) ->
           let parts = Span.to_parts time in
           let ofday = Time.Ofday.create ~hr:parts.hr ~min:parts.min () in
           Time.of_date_ofday ~zone date ofday)

  (** Returns next occurrence *)
  let next ?start_from schedule : Time.t =
    let zone = Lazy.force Zone.local in

    let s = ToTime.schedule_to_time schedule in
    let () = U.debug_schedule s in

    let today = Option.value start_from ~default:(Time.now ()) in
    let date = Time.to_date ~zone today in

    let times =
      let hours_and_minutes = List.cartesian_product s.hour s.minute in
      List.map hours_and_minutes ~f:(fun (hr, min) -> Span.( + ) hr min)
    in

    let next_datetime schedule times : Time.t = Time.now () in

    let rec find_nearest_datetime next_datetime nearest_to : Time.t =
      let datetime, next_datetime =
        Option.value_exn (Sequence.next next_datetime)
      in
      if Time.( >= ) datetime nearest_to then datetime
      else find_nearest_datetime next_datetime nearest_to
    in

    Time.epoch

  (* let dates =
      calculate_dates ~skip_until:(Date.month date) (Date.year date) s
    in
    let () = U.print_debug "dates" [%sexp_of: Date.t list] dates in

    (* Find the first datetime that is >= today *)
    let datetimes = merge_dates_and_times zone dates times in
    match List.find datetimes ~f:(fun dt -> Time.( >= ) dt today) with
    | Some next_time -> next_time
    | None ->
        (* If no match in current year, try next year *)
        let next_year = Date.year date + 1 in
        let new_dates = calculate_dates next_year s in
        let datetimes = merge_dates_and_times zone new_dates times in
        List.hd_exn datetimes *)
end

let parse = Parse.parse
let next = Schedule.next
