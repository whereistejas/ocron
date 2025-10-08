open Core
open Ocron
module Time = Time_float_unix
module Span = Time.Span

let zone = Lazy.force Time.Zone.local

(* Get path to main.py in project root *)
let main_py_path () = sprintf "%s/main.py" (Sys.getenv_exn "DUNE_SOURCEROOT")

(* Format time as YYYY-MM-DD HH:MM for command line *)
let format_time_for_cli time =
  let date = Time.to_date ~zone time in
  let ofday = Time.to_ofday ~zone time in
  let parts = Time.Ofday.to_parts ofday in
  sprintf "%04d-%02d-%02d %02d:%02d" (Date.year date)
    (Month.to_int (Date.month date))
    (Date.day date) parts.hr parts.min

(* Build command to run main.py *)
let build_command cron_expr start_time count =
  sprintf "uv run python %s --start_from %s --count %d %s" (main_py_path ())
    (Filename.quote (format_time_for_cli start_time))
    count (Filename.quote cron_expr)

(* Check if line is an error *)
let is_error_line line =
  String.is_prefix line ~prefix:"ERROR:"
  || String.is_prefix line ~prefix:"Traceback"

(* Parse a single datetime line from main.py output *)
let parse_datetime_line line =
  let stripped = String.strip line in
  if String.is_empty stripped then None
  else try Some (Time.of_string stripped) with _ -> None

(* Process lines from command output, separating results and errors *)
let process_lines ic =
  let rec loop results errors =
    match In_channel.input_line ic with
    | None -> (List.rev results, List.rev errors)
    | Some line -> (
        if is_error_line line then loop results (line :: errors)
        else
          match parse_datetime_line line with
          | Some time -> loop (time :: results) errors
          | None ->
              if String.is_empty (String.strip line) then loop results errors
              else loop results (line :: errors))
  in
  loop [] []

(* Get next N occurrences from main.py *)
let get_croniter_results cron_expr start_time count =
  let cmd = build_command cron_expr start_time count in

  try
    let ic = UnixLabels.open_process_in cmd in
    let results, errors = process_lines ic in
    let _ = UnixLabels.close_process_in ic in

    match errors with
    | [] -> results
    | _ ->
        failwith (sprintf "croniter error: %s" (String.concat ~sep:"\n" errors))
  with
  | Failure msg -> raise (Failure msg)
  | _ -> raise (Failure "main.py not available or croniter not installed")

(* Check if main.py is available and working *)
let check_main_py_available () =
  let check_cmd =
    sprintf "uv run python %s --count 1 '0 0 * * *' 2>/dev/null >/dev/null"
      (main_py_path ())
  in
  match Core_unix.system check_cmd with
  | Ok () -> true
  | Error _ ->
      printf
        "\nSkipping tests: main.py not available or croniter not installed\n";
      false

(* Create a test start time *)
let make_start_time ~year ~month ~day ~hour ~minute =
  let date = Date.create_exn ~y:year ~m:month ~d:day in
  let ofday = Time.Ofday.create ~hr:hour ~min:minute () in
  Time.of_date_ofday ~zone date ofday

(* Format list of times for display *)
let format_times times =
  List.map times ~f:(Time.to_string_abs ~zone) |> String.concat ~sep:"\n    "

(* Compare two lists of times and format error message if different *)
let compare_results cron_expr start_from expected actual =
  if List.equal Time.equal actual expected then Ok ()
  else
    Error
      (sprintf
         "Test failed for cron: %s\n\
         \  Start: %s\n\
         \  Expected (croniter):\n\
         \    %s\n\
         \  Actual (ocron):\n\
         \    %s"
         cron_expr
         (Time.to_string_abs ~zone start_from)
         (format_times expected) (format_times actual))

(* Test a single cron expression *)
let test_cron_expr cron_expr ~start_from ~(count : int) =
  let upcoming_times = upcoming ~start_from ~count:5 (parse cron_expr) in
  let actual = List.take upcoming_times count in
  let expected = get_croniter_results cron_expr start_from count in
  compare_results cron_expr start_from expected actual

(* Test cases *)
let test_cases = [ "30 14 * * *"; "0 0 * * 1" ]

let%test_unit "compare against croniter reference implementation" =
  if check_main_py_available () then
    let start_from =
      make_start_time ~year:2025 ~month:Month.jan ~day:15 ~hour:10 ~minute:0
    in
    let count = 5 in

    List.iter test_cases ~f:(fun cron_expr ->
        match test_cron_expr cron_expr ~start_from ~count with
        | Ok () -> ()
        | Error msg -> failwith msg)
