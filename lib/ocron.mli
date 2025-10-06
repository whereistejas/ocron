(** OCron - A cron expression parser and scheduler for OCaml *)

open Core
module Time = Time_float_unix
module Span = Time.Span

(** A cron element can be either a single value or a range *)
type 'span element =
  | Single of 'span
  | Range of {
      max : 'span;
      min : 'span;
    }
[@@deriving sexp, compare]

(** A cron value can be all values (<asterik>), a single element, or a list of
    elements *)
type 'span value =
  | All
  | Value of 'span element
  | List of 'span element list
[@@deriving sexp, compare]

type schedule = {
  minute : Span.t value;  (** Valid values: [0,59] *)
  hour : Span.t value;  (** Valid values: [0,23] *)
  day_of_month : Span.t value;  (** Valid values: [1,31] *)
  month : Month.t value;  (** Valid values: [1,12] *)
  day_of_week : Day_of_week.t value;  (** Valid values: [0,6] with 0=Sunday *)
}
[@@deriving sexp, compare]
(** A complete cron schedule specification *)

val parse : string -> schedule
(** Parse a cron expression string into a schedule.

    The cron expression should be in standard format:
    [minute hour day_of_month month day_of_week]

    Example: "0 9 * * 1" represents "At 9:00 AM on Monday"

    @raise Invalid_argument if the cron expression is malformed *)

val next : ?start_from:Time.t -> schedule -> Time.t
(** Calculate the next occurrence time for a given schedule.

    @param start_from Optional starting time (defaults to current time)
    @param schedule The cron schedule to evaluate
    @return The next time the schedule will occur *)
