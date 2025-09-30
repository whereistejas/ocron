open Ocron

let%test_unit _ =
  let actual = Schedule.parse "* * * * *" in
  let expected : Schedule.t =
    {
      minute = All;
      hour = All;
      day_of_the_month = All;
      month_of_the_year = All;
      day_of_the_week = All;
    }
  in
  [%test_eq: Schedule.t] actual expected
