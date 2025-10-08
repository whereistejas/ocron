open Core
open Sexplib.Std
module Time = Time_float_unix
module Span = Time.Span

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

type expr = {
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

type type_of_day =
  | Day_of_week of Day_of_week.t
  | Day_of_month of int
[@@deriving sexp, compare]

type partial_date = {
  month : Month.t;
  day : type_of_day;
}
[@@deriving sexp, compare]

type schedule = {
  spans : Span.t list;
  dates : partial_date list;
}
[@@deriving sexp, compare]

let max_minute : int = 59
let min_minute : int = 0
let max_hour : int = 23
let min_hour : int = 0
let max_day_of_month : int = 31
let min_day_of_month : int = 1

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
  let parse_element (value : string) (int_to_time : int -> 'time) :
      'time element =
    match value with
    | value when String.contains value '-' ->
        String.lsplit2 ~on:'-' value
        |> Option.map ~f:(fun (min, max) ->
               let min = int_of_string min in
               let max = int_of_string max in
               Range { max = int_to_time max; min = int_to_time min })
        |> Option.value_exn
    | value ->
        let value = int_of_string value in
        Single (int_to_time value)

  (** Each of these patterns can be either an asterisk (meaning all valid *
      values), an element, or a list of elements separated by commas. *)
  let parse_value (value : string) (int_to_time : int -> 'time) : 'time value =
    match value with
    | "*" -> All
    | value when String.contains value ',' ->
        let elements =
          String.split value ~on:','
          |> List.map ~f:(fun value -> parse_element value int_to_time)
        in
        List elements
    | value -> Value (parse_element value int_to_time)

  let parse (value : string) : expr =
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

module Schedule = struct
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

  let element_to_day_of_week (element : 'time element) : Day_of_week.t list =
    match element with
    | Single el -> [ el ]
    | Range { min; max } ->
        List.filter Day_of_week.all ~f:(fun day_of_week ->
            Day_of_week.( >= ) min day_of_week
            && Day_of_week.( >= ) day_of_week max)

  let value_to_day_of_week (value : 'time value) : Day_of_week.t list =
    match value with
    | All -> Day_of_week.all
    | Value el -> element_to_day_of_week el
    | List els -> List.concat_map els ~f:(fun el -> element_to_day_of_week el)

  (** Convert cron syntax to types that correspond to the time units *)
  let expr_to_schedule expr : schedule =
    let { minute; hour; day_of_month; month; day_of_week } = expr in
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
    let day_of_week = value_to_day_of_week day_of_week in

    let spans =
      List.cartesian_product hour minute
      |> List.map ~f:(fun (hr, min) -> Span.( + ) hr min)
    in

    (* TODO: If only day of week is specified, ignore day of month and vice-versa *)
    let dates =
      let day_of_month =
        List.cartesian_product month day_of_month
        |> List.map ~f:(fun (month, day) ->
               { month; day = Day_of_month (Span.to_day day |> int_of_float) })
      in
      let day_of_week =
        List.cartesian_product month day_of_week
        |> List.map ~f:(fun (month, day_of_week) ->
               { month; day = Day_of_week day_of_week })
      in
      List.append day_of_month day_of_week
    in

    { spans; dates }

  let partial_date_to_date ~year partial_date : Date.t list =
    let { month; day } = partial_date in

    let max_days = Date.days_in_month ~year ~month in

    match day with
    | Day_of_month day ->
        if day <= max_days then [ Date.create_exn ~d:day ~m:month ~y:year ]
        else []
    | Day_of_week day_of_week ->
        List.range ~stop:`inclusive 1 max_days
        |> List.filter_map ~f:(fun day ->
               let date = Date.create_exn ~d:day ~m:month ~y:year in
               if Day_of_week.(Date.day_of_week date = day_of_week) then
                 Some date
               else None)

  let upcoming ~start_from ~count schedule : Time.t list =
    let zone = Lazy.force Time.Zone.local in
    let year = Time.to_date ~zone start_from |> Date.year in

    let { spans; dates } = schedule in

    let dates =
      List.concat_map dates ~f:(fun date -> partial_date_to_date ~year date)
    in
    let dates_and_spans = List.cartesian_product dates spans in
    let filtered_dates_and_spans =
      List.filter_map dates_and_spans ~f:(fun (date, span) ->
          let ofday = Time.Ofday.of_span_since_start_of_day_exn span in
          let time = Time.of_date_ofday ~zone date ofday in
          if Time.(time >= start_from) then Some time else None)
    in
    List.take filtered_dates_and_spans count
end

let parse = Parse.parse

let rec upcoming ?(start_from = Time.now ()) ~count cron_expr : Time.t list =
  let schedule = Schedule.expr_to_schedule cron_expr in
  let upcoming_times = Schedule.upcoming ~start_from ~count schedule in
  if List.length upcoming_times < count then
    let last = List.last_exn upcoming_times in
    let later =
      upcoming ~start_from:last
        ~count:(count - List.length upcoming_times)
        cron_expr
    in
    List.append upcoming_times later
  else List.take upcoming_times count
