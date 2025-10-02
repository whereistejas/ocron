open Ocron
open Core
module Time = Time_float_unix
module Span = Time.Span
module Zone = Time.Zone

let%test_unit _ =
  let actual = parse_schedule "2 * * * *" in
  let expected : schedule =
    {
      minute = Value (Single (Span.create ~min:2 ()));
      hour = All;
      day_of_month = All;
      month = All;
      day_of_week = All;
    }
  in
  [%test_eq: schedule] actual expected

let%test_unit _ =
  let actual = parse_schedule "2-5 * * * *" in
  let expected : schedule =
    {
      minute =
        Value
          (Range { min = Span.create ~min:2 (); max = Span.create ~min:5 () });
      hour = All;
      day_of_month = All;
      month = All;
      day_of_week = All;
    }
  in
  [%test_eq: schedule] actual expected

let%test_unit _ =
  let actual = parse_schedule "2,3,5 * * * *" in
  let expected : schedule =
    {
      minute =
        List
          [
            Single (Span.create ~min:2 ());
            Single (Span.create ~min:3 ());
            Single (Span.create ~min:5 ());
          ];
      hour = All;
      day_of_month = All;
      month = All;
      day_of_week = All;
    }
  in
  [%test_eq: schedule] actual expected

let%test_unit _ =
  let actual = parse_schedule "2,3,5-9 * * 12 *" in
  let expected : schedule =
    {
      minute =
        List
          [
            Single (Span.create ~min:2 ());
            Single (Span.create ~min:3 ());
            Range { min = Span.create ~min:5 (); max = Span.create ~min:9 () };
          ];
      hour = All;
      day_of_month = All;
      month = Value (Single Month.dec);
      day_of_week = All;
    }
  in
  [%test_eq: schedule] actual expected

let%test_unit _ =
  let start_from = Time.now () in
  let schedule = parse_schedule "2 * * * *" in
  let actual = List.hd_exn (next schedule ~start_from) in
  let expected =
    let zone = Lazy.force Zone.local in
    let date = Time.to_date ~zone start_from in
    let ofday = Time.to_ofday ~zone start_from in
    let parts = Time.Ofday.to_parts ofday in
    let ofday = Time.Ofday.create ~hr:(parts.hr + 1) ~min:2 () in
    Time.of_date_ofday ~zone date ofday
  in
  [%test_eq: Time.t] actual expected
