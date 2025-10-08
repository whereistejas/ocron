open Core
open Ocron
module Time = Time_float_unix
module Span = Time.Span

(* Helper to get next N occurrences from croniter *)
let get_croniter_results cron_expr start_time count =
  let zone = Lazy.force Time.Zone.local in

  let make_time year month day hr min =
    let date = Date.create_exn ~y:year ~m:month ~d:day in
    let ofday = Time.Ofday.create ~hr ~min () in
    Time.of_date_ofday ~zone date ofday
  in
  let python_script =
    Printf.sprintf
      {|
import sys
from croniter import croniter
from datetime import datetime

cron_expr = '%s'
start_time = datetime(%d, %d, %d, %d, %d)
count = %d

try:
    cron = croniter(cron_expr, start_time)
    for _ in range(count):
        next_time = cron.get_next(datetime)
        print(f"{next_time.year},{next_time.month},{next_time.day},{next_time.hour},{next_time.minute}")
except Exception as e:
    print(f"ERROR: {e}", file=sys.stderr)
    sys.exit(1)
|}
      cron_expr
      (Date.year (Time.to_date ~zone start_time))
      (Month.to_int (Date.month (Time.to_date ~zone start_time)))
      (Date.day (Time.to_date ~zone start_time))
      (Time.Ofday.to_parts (Time.to_ofday ~zone start_time)).hr
      (Time.Ofday.to_parts (Time.to_ofday ~zone start_time)).min count
  in

  let cmd =
    Printf.sprintf "uv run python3 -c %s" (Filename.quote python_script)
  in

  try
    let ic = UnixLabels.open_process_in cmd in
    let results = ref [] in
    let error_lines = ref [] in
    try
      while true do
        match In_channel.input_line ic with
        | Some line -> (
            if String.is_prefix line ~prefix:"ERROR:" then
              error_lines := line :: !error_lines
            else if String.is_prefix line ~prefix:"Traceback" then
              error_lines := line :: !error_lines
            else
              let parts = String.split_on_chars line ~on:[ ',' ] in
              match parts with
              | [ year; month; day; hour; minute ] ->
                  let t =
                    make_time (Int.of_string year)
                      (Month.of_int_exn (Int.of_string month))
                      (Int.of_string day) (Int.of_string hour)
                      (Int.of_string minute)
                  in
                  results := t :: !results
              | _ when not (String.is_empty (String.strip line)) ->
                  error_lines := line :: !error_lines
              | _ -> ())
        | None -> raise End_of_file
      done;
      []
    with End_of_file ->
      ignore (UnixLabels.close_process_in ic);
      if not (List.is_empty !error_lines) then
        failwith
          (Printf.sprintf "croniter error: %s"
             (String.concat ~sep:"\n" (List.rev !error_lines)))
      else List.rev !results
  with
  | Failure msg -> raise (Failure msg)
  | _ ->
      raise
        (Failure
           "croniter not available - install with: uv pip install croniter")

(* Test cases: list of cron expressions to test *)
let test_cases = [ "30 14 * * *"; "0 0 * * 1" ]

let%test_unit "compare against croniter reference implementation" =
  let zone = Lazy.force Time.Zone.local in
  let count = 5 in

  (* Check if croniter is available first *)
  let has_croniter =
    let check_cmd = "uv run python3 -c 'import croniter' 2>/dev/null" in
    match Core_unix.system check_cmd with
    | Ok () -> true
    | Error _ ->
        Printf.printf
          "\n\
           Skipping croniter tests: croniter not installed (uv pip install \
           croniter)\n";
        false
  in

  if has_croniter then
    let start_from =
      let date = Date.create_exn ~y:2025 ~m:Month.jan ~d:15 in
      let ofday = Time.Ofday.create ~hr:10 ~min:0 () in
      Time.of_date_ofday ~zone date ofday
    in

    List.iter test_cases ~f:(fun cron_expr ->
        let schedule = parse cron_expr in
        let actual_list =
          let seq = upcoming schedule ~start_from in
          let taken = Sequence.take seq count in
          Sequence.to_list taken
        in

        let expected_list = get_croniter_results cron_expr start_from count in

        if not (List.equal Time.equal actual_list expected_list) then
          let format_times times =
            List.map times ~f:(Time.to_string_abs ~zone)
            |> String.concat ~sep:"\n    "
          in
          failwith
            (Printf.sprintf
               "Test failed for cron: %s\n\
               \  Start: %s\n\
               \  Expected (croniter):\n\
               \    %s\n\
               \  Actual (ocron):\n\
               \    %s"
               cron_expr
               (Time.to_string_abs ~zone start_from)
               (format_times expected_list)
               (format_times actual_list)))
