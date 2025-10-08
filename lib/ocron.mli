open Core
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
  minute : Span.t value;
  hour : Span.t value;
  day_of_month : Span.t value;
  month : Month.t value;
  day_of_week : Day_of_week.t value;
}
[@@deriving sexp, compare]

val parse : string -> expr
val upcoming : ?start_from:Time.t -> count:int -> expr -> Time.t list
