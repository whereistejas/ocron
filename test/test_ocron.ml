open Ocron

let%test_unit _ =
  let actual = Schedule.parse "0 0 0 0 0" |> Option.get in
  let expected : Schedule.t =
    {
      minute = Value (Element Zero);
      hour = Value (Element Zero);
      day_of_the_month = Value (Element One);
      month_of_the_year = Value (Element January);
      day_of_the_week = Value (Element Sunday);
    }
  in
  [%test_eq: Schedule.t] actual expected
