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
        |> Option.value_exn ~message:"Could not parse element with list syntax"
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
    let day_of_month =
      parse_value day_str (fun day -> Time.Span.create ~day ())
    in
    let month =
      parse_value month_str (fun month ->
          try Month.of_int_exn month
          with e ->
            let msg = Exn.to_string e and stack = Printexc.get_backtrace () in
            Printf.eprintf "there was an error converting int to month: %s%s\n"
              msg stack;
            raise e)
    in
    let day_of_week =
      parse_value dow_str (fun day ->
          try Day_of_week.of_int_exn day
          with e ->
            let msg = Exn.to_string e and stack = Printexc.get_backtrace () in
            Printf.eprintf "there was an error converting int to month: %s%s\n"
              msg stack;
            raise e)
    in
    { minute; hour; day_of_month; month; day_of_week }
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

  let element_to_time ~(to_time : int -> 'time) ~(to_int : 'time -> int)
      ~(next_time : 'time -> 'time) ~(compare_time : 'time -> 'time -> int)
      (element : 'time element) : 'time list =
    match element with
    | Single el -> [ el ]
    | Range { min; max } ->
        if compare_time min max = 0 then
          List.range (next_time min |> to_int) (to_int max) ~stop:`inclusive
          |> List.map ~f:to_time
        else
          List.range (to_int min) (to_int max) ~stop:`inclusive
          |> List.map ~f:to_time

  let value_to_time ~(min : int) ~(max : int) ~(to_time : int -> 'time)
      ~(to_int : 'time -> int) ~(next_time : 'time -> 'time)
      ~(compare_time : 'time -> 'time -> int) (value : 'time value) : 'time list
      =
    match value with
    | All -> List.range min max ~stop:`inclusive |> List.map ~f:to_time
    | Value el -> element_to_time ~to_time ~to_int ~next_time ~compare_time el
    | List els ->
        List.concat_map els
          ~f:(element_to_time ~to_time ~to_int ~next_time ~compare_time)

  let expr_to_schedule expr : schedule =
    let { minute; hour; day_of_month; month; day_of_week } = expr in
    let spans =
      let minute =
        value_to_time ~min:0 ~max:59
          ~to_time:(fun int -> Span.create ~min:int ())
          ~to_int:(fun time -> Span.to_min time |> int_of_float)
          ~next_time:(fun time -> Span.next time)
          ~compare_time:(fun a b -> Span.compare a b)
          minute
      in
      let hour =
        value_to_time ~min:0 ~max:23
          ~to_time:(fun int -> Span.create ~hr:int ())
          ~to_int:(fun time -> Span.to_hr time |> int_of_float)
          ~next_time:(fun time -> Span.next time)
          ~compare_time:(fun a b -> Span.compare a b)
          hour
      in
      List.cartesian_product hour minute
      |> List.map ~f:(fun (hr, min) -> Span.( + ) hr min)
    in
    let dates =
      let month =
        value_to_time ~min:1 ~max:12
          ~to_time:(fun int -> Month.of_int_exn int)
          ~to_int:(fun time -> Month.to_int time)
          ~next_time:(fun time -> Month.shift time 1)
          ~compare_time:(fun a b -> Month.compare a b)
          month
      in
      let calculate_day_of_month (dom : Span.t value) =
        let day_of_month =
          value_to_time ~min:1 ~max:31
            ~to_time:(fun int -> Span.create ~day:int ())
            ~to_int:(fun time -> Span.to_day time |> int_of_float)
            ~next_time:(fun time -> Span.next time)
            ~compare_time:(fun a b -> Span.compare a b)
            dom
        in
        List.cartesian_product month day_of_month
        |> List.map ~f:(fun (month, day) ->
               { month; day = Day_of_month (Span.to_day day |> int_of_float) })
      in
      let calculate_day_of_week (dow : Day_of_week.t value) =
        let day_of_week =
          value_to_time ~min:0 ~max:6
            ~to_time:(fun int -> Day_of_week.of_int_exn int)
            ~to_int:(fun time -> Day_of_week.to_int time)
            ~next_time:(fun time -> Day_of_week.shift time 1)
            ~compare_time:(fun a b -> Day_of_week.compare a b)
            dow
        in
        List.cartesian_product month day_of_week
        |> List.map ~f:(fun (month, day_of_week) ->
               { month; day = Day_of_week day_of_week })
      in
      let day_of_month, day_of_week =
        match (day_of_month, day_of_week) with
        | All, All | _, All -> (calculate_day_of_month day_of_month, [])
        | All, _ -> ([], calculate_day_of_week day_of_week)
        | _, _ ->
            let day_of_month = calculate_day_of_month day_of_month in
            let day_of_week = calculate_day_of_week day_of_week in
            (day_of_month, day_of_week)
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

  (** One pass covers all datetime's between start_from and end of year *)
  let upcoming ~start_from schedule : Time.t list =
    (* let () =
      Printf.eprintf "schedule: %s\n"
        (Sexp.to_string_hum (sexp_of_schedule schedule))
    in *)
    let zone = Lazy.force Time.Zone.local in
    let year = Time.to_date ~zone start_from |> Date.year in

    let { spans; dates } = schedule in

    let dates =
      List.concat_map dates ~f:(fun date -> partial_date_to_date ~year date)
      |> List.sort ~compare:(fun a b -> Date.(compare a b))
    in
    let dates_and_spans = List.cartesian_product dates spans in
    List.filter_map dates_and_spans ~f:(fun (date, span) ->
        let ofday =
          try Time.Ofday.of_span_since_start_of_day_exn span
          with e ->
            let msg = Exn.to_string e and stack = Printexc.get_backtrace () in
            Printf.eprintf "there was an error converting span to ofday: %s%s\n"
              msg stack;
            raise e
        in
        let time = Time.of_date_ofday ~zone date ofday in
        if Time.(time >= start_from) then Some time else None)
end

let parse = Parse.parse

let rec upcoming ?(start_from = Time.now ()) ~count cron_expr : Time.t list =
  (* let () =
    Printf.eprintf "cron_expr: %s\n"
      (Sexp.to_string_hum (sexp_of_expr cron_expr))
  in *)
  let schedule = Schedule.expr_to_schedule cron_expr in
  let upcoming_times =
    let times = Schedule.upcoming ~start_from schedule in
    (* let () =
      Printf.eprintf "times: %s\n"
        (Sexp.to_string_hum (List.sexp_of_t Time.sexp_of_t times))
    in *)
    if List.length times < count then
      let zone = Lazy.force Time.Zone.local in
      let year = Time.to_date ~zone start_from |> Date.year in
      let day = Date.create_exn ~y:(year + 1) ~m:Month.Jan ~d:1 in
      let ofday = Time.Ofday.start_of_day in
      let start_from = Time.of_date_ofday ~zone day ofday in
      let more_times =
        upcoming ~start_from ~count:(count - List.length times) cron_expr
      in
      List.append times more_times
    else times
  in
  let upcoming_times =
    List.dedup_and_sort upcoming_times ~compare:Time.compare
  in
  List.take upcoming_times count
