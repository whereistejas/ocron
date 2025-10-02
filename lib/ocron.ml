open Base
open Sexplib.Std

(*
In the POSIX locale, the user or application shall ensure that a crontab
entry is a text file consisting of lines of six fields each. The fields
shall be separated by <blank>s. The first five fields shall be integer
patterns that specify the following:
1. Minute [0,59]
2. Hour [0,23]
3. Day of the month [1,31]
4. Month of the year [1,12]
5. Day of the week ([0,6] with 0=Sunday)
*)
module Time = struct
  module Minute = struct
    type t =
      | Zero
      | One
      | Two
      | Three
      | Four
      | Five
      | Six
      | Seven
      | Eight
      | Nine
      | Ten
      | Eleven
      | Twelve
      | Thirteen
      | Fourteen
      | Fifteen
      | Sixteen
      | Seventeen
      | Eighteen
      | Nineteen
      | Twenty
      | TwentyOne
      | TwentyTwo
      | TwentyThree
      | TwentyFour
      | TwentyFive
      | TwentySix
      | TwentySeven
      | TwentyEight
      | TwentyNine
      | Thirty
      | ThirtyOne
      | ThirtyTwo
      | ThirtyThree
      | ThirtyFour
      | ThirtyFive
      | ThirtySix
      | ThirtySeven
      | ThirtyEight
      | ThirtyNine
      | Forty
      | FortyOne
      | FortyTwo
      | FortyThree
      | FortyFour
      | FortyFive
      | FortySix
      | FortySeven
      | FortyEight
      | FortyNine
      | Fifty
      | FiftyOne
      | FiftyTwo
      | FiftyThree
      | FiftyFour
      | FiftyFive
      | FiftySix
      | FiftySeven
      | FiftyEight
      | FiftyNine
    [@@deriving sexp, compare, show]

    let parse minute =
      match minute with
      | "0" -> Zero
      | "1" -> One
      | "2" -> Two
      | "3" -> Three
      | "4" -> Four
      | "5" -> Five
      | "6" -> Six
      | "7" -> Seven
      | "8" -> Eight
      | "9" -> Nine
      | "10" -> Ten
      | "11" -> Eleven
      | "12" -> Twelve
      | "13" -> Thirteen
      | "14" -> Fourteen
      | "15" -> Fifteen
      | "16" -> Sixteen
      | "17" -> Seventeen
      | "18" -> Eighteen
      | "19" -> Nineteen
      | "20" -> Twenty
      | "21" -> TwentyOne
      | "22" -> TwentyTwo
      | "23" -> TwentyThree
      | "24" -> TwentyFour
      | "25" -> TwentyFive
      | "26" -> TwentySix
      | "27" -> TwentySeven
      | "28" -> TwentyEight
      | "29" -> TwentyNine
      | "30" -> Thirty
      | "31" -> ThirtyOne
      | "32" -> ThirtyTwo
      | "33" -> ThirtyThree
      | "34" -> ThirtyFour
      | "35" -> ThirtyFive
      | "36" -> ThirtySix
      | "37" -> ThirtySeven
      | "38" -> ThirtyEight
      | "39" -> ThirtyNine
      | "40" -> Forty
      | "41" -> FortyOne
      | "42" -> FortyTwo
      | "43" -> FortyThree
      | "44" -> FortyFour
      | "45" -> FortyFive
      | "46" -> FortySix
      | "47" -> FortySeven
      | "48" -> FortyEight
      | "49" -> FortyNine
      | "50" -> Fifty
      | "51" -> FiftyOne
      | "52" -> FiftyTwo
      | "53" -> FiftyThree
      | "54" -> FiftyFour
      | "55" -> FiftyFive
      | "56" -> FiftySix
      | "57" -> FiftySeven
      | "58" -> FiftyEight
      | "59" -> FiftyNine
      | _ -> raise (Invalid_argument ("Invalid minute value: " ^ minute))
  end

  module Hour = struct
    type t =
      | Zero
      | One
      | Two
      | Three
      | Four
      | Five
      | Six
      | Seven
      | Eight
      | Nine
      | Ten
      | Eleven
      | Twelve
      | Thirteen
      | Fourteen
      | Fifteen
      | Sixteen
      | Seventeen
      | Eighteen
      | Nineteen
      | Twenty
      | TwentyOne
      | TwentyTwo
      | TwentyThree
    [@@deriving sexp, compare, show]

    let parse hour =
      match hour with
      | "0" -> Zero
      | "1" -> One
      | "2" -> Two
      | "3" -> Three
      | "4" -> Four
      | "5" -> Five
      | "6" -> Six
      | "7" -> Seven
      | "8" -> Eight
      | "9" -> Nine
      | "10" -> Ten
      | "11" -> Eleven
      | "12" -> Twelve
      | "13" -> Thirteen
      | "14" -> Fourteen
      | "15" -> Fifteen
      | "16" -> Sixteen
      | "17" -> Seventeen
      | "18" -> Eighteen
      | "19" -> Nineteen
      | "20" -> Twenty
      | "21" -> TwentyOne
      | "22" -> TwentyTwo
      | "23" -> TwentyThree
      | _ -> raise (Invalid_argument ("Invalid hour value: " ^ hour))
  end

  module DayOfTheMonth = struct
    type t =
      | One
      | Two
      | Three
      | Four
      | Five
      | Six
      | Seven
      | Eight
      | Nine
      | Ten
      | Eleven
      | Twelve
      | Thirteen
      | Fourteen
      | Fifteen
      | Sixteen
      | Seventeen
      | Eighteen
      | Nineteen
      | Twenty
      | TwentyOne
      | TwentyTwo
      | TwentyThree
      | TwentyFour
      | TwentyFive
      | TwentySix
      | TwentySeven
      | TwentyEight
      | TwentyNine
      | Thirty
      | ThirtyOne
    [@@deriving sexp, compare, show]

    let parse day =
      match day with
      | "1" -> One
      | "2" -> Two
      | "3" -> Three
      | "4" -> Four
      | "5" -> Five
      | "6" -> Six
      | "7" -> Seven
      | "8" -> Eight
      | "9" -> Nine
      | "10" -> Ten
      | "11" -> Eleven
      | "12" -> Twelve
      | "13" -> Thirteen
      | "14" -> Fourteen
      | "15" -> Fifteen
      | "16" -> Sixteen
      | "17" -> Seventeen
      | "18" -> Eighteen
      | "19" -> Nineteen
      | "20" -> Twenty
      | "21" -> TwentyOne
      | "22" -> TwentyTwo
      | "23" -> TwentyThree
      | "24" -> TwentyFour
      | "25" -> TwentyFive
      | "26" -> TwentySix
      | "27" -> TwentySeven
      | "28" -> TwentyEight
      | "29" -> TwentyNine
      | "30" -> Thirty
      | "31" -> ThirtyOne
      | _ -> raise (Invalid_argument ("Invalid day value: " ^ day))
  end

  module MonthOfTheYear = struct
    type t =
      | January
      | February
      | March
      | April
      | May
      | June
      | July
      | August
      | September
      | October
      | November
      | December
    [@@deriving sexp, compare, show]

    let parse month =
      match month with
      | "1" -> January
      | "2" -> February
      | "3" -> March
      | "4" -> April
      | "5" -> May
      | "6" -> June
      | "7" -> July
      | "8" -> August
      | "9" -> September
      | "10" -> October
      | "11" -> November
      | "12" -> December
      | _ -> raise (Invalid_argument ("Invalid month value: " ^ month))
  end

  module DayOfTheWeek = struct
    type t =
      | Sunday
      | Monday
      | Tuesday
      | Wednesday
      | Thursday
      | Friday
      | Saturday
    [@@deriving sexp, compare, show]

    let parse day =
      match day with
      | "0" -> Sunday
      | "1" -> Monday
      | "2" -> Tuesday
      | "3" -> Wednesday
      | "4" -> Thursday
      | "5" -> Friday
      | "6" -> Saturday
      | _ -> raise (Invalid_argument ("Invalid day of the week value: " ^ day))
  end
end

(*
The specification of days can be made by two fields (day of the month and
day of the week). If month, day of month, and day of week are all asterisks,
every day shall be matched. If either the month or day of month is specified
as an element or list, but the day of week is an asterisk, the month and day
of month fields shall specify the days that match. If both month and day of
month are specified as an asterisk, but day of week is an element or list,
then only the specified days of the week match.

Finally, if either the month or day of month is specified as an element or
list, and the day of week is also specified as an element or list, then any
day matching either the month and day of month, or the day of week, shall
be matched.
*)
module Schedule = struct
  open Time

  (* An element shall be either a number or two numbers separated by
  a hyphen (meaning an inclusive range). *)
  type 'time element =
    | Single of 'time
    | Range of {
        max : 'time;
        min : 'time;
      }
  [@@deriving sexp, compare, show]

  (* Each of these patterns can be either an asterisk (meaning
  all valid values), an element, or a list of elements separated
  by commas. *)
  and 'time value =
    | All
    | Value of 'time element
    | List of 'time element list
  [@@deriving sexp, compare, show]

  and t = {
    minute : Minute.t value;
    hour : Hour.t value;
    day_of_the_month : DayOfTheMonth.t value;
    month_of_the_year : MonthOfTheYear.t value;
    day_of_the_week : DayOfTheWeek.t value;
  }
  [@@deriving sexp, compare, show]

  let parse_element (value : string) (parse : string -> 'time) : 'time element =
    match value with
    | value when String.contains value '-' ->
        String.lsplit2 ~on:'-' value
        |> Option.map ~f:(fun (min, max) ->
               Range { max = parse max; min = parse min })
        |> Option.value_exn
    | value -> Single (parse value)

  let parse_value (value : string) (parse : string -> 'time) : 'time value =
    match value with
    | "*" -> All
    | value when String.contains value ',' ->
        let elements =
          String.split value ~on:','
          |> List.map ~f:(fun value -> parse_element value parse)
        in
        List elements
    | value -> Value (parse_element value parse)

  let tokenise value : string * string * string * string * string =
    let fields =
      String.split_on_chars value ~on:[ ' '; '\t' ]
      |> List.filter_map ~f:(fun s ->
             let stripped = String.strip s in
             let is_empty = String.is_empty stripped in
             Option.some_if (not is_empty) stripped)
    in
    match fields with
    | [ minute_str; hour_str; day_str; month_str; dow_str ] ->
        (minute_str, hour_str, day_str, month_str, dow_str)
    | _ -> raise (Invalid_argument "Invalid cron expression")

  let parse (value : string) : t =
    let minute_str, hour_str, day_str, month_str, dow_str = tokenise value in
    let minute = parse_value minute_str Minute.parse in
    let hour = parse_value hour_str Hour.parse in
    let day_of_the_month = parse_value day_str DayOfTheMonth.parse in
    let month_of_the_year = parse_value month_str MonthOfTheYear.parse in
    let day_of_the_week = parse_value dow_str DayOfTheWeek.parse in
    { minute; hour; day_of_the_month; month_of_the_year; day_of_the_week }
end
