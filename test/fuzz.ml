open Core
open Ocron
module Time = Time_float_unix
module Span = Time.Span

let zone = Lazy.force Time.Zone.local

(* Enable detailed backtraces *)
let () = Printexc.record_backtrace true

(* Helper for printf with automatic newline *)
let printfn fmt = ksprintf print_endline fmt

(* Cron expression grammar-based generator *)
module CronGen = struct
  (* Field ranges for POSIX cron *)
  type field_range = {
    min : int;
    max : int;
    name : string;
  }

  let minute = { min = 0; max = 59; name = "minute" }
  let hour = { min = 0; max = 23; name = "hour" }
  let day_of_month = { min = 1; max = 31; name = "day_of_month" }
  let month = { min = 1; max = 12; name = "month" }
  let day_of_week = { min = 0; max = 6; name = "day_of_week" }

  (* Generate a random value in range *)
  let random_in_range range = Random.int_incl range.min range.max

  (* Generate a single field value *)
  let gen_single_value range = string_of_int (random_in_range range)

  (* Generate a wildcard *)
  let gen_wildcard _range = "*"

  (* Generate a range expression *)
  let gen_range range =
    let max_start = max range.min (range.max - 1) in
    let start = Random.int_incl range.min max_start in
    let end_ = Random.int_incl (min (start + 1) range.max) range.max in
    sprintf "%d-%d" start end_

  (* (* Generate a step expression *)
  let gen_step range =
    let step = Random.int_incl 2 (max 2 ((range.max - range.min) / 2)) in
    if Random.bool () then sprintf "*/%d" step
    else
      let start = random_in_range range in
      let end_ = Random.int_incl start range.max in
      sprintf "%d-%d/%d" start end_ step *)

  (* Generate a list expression *)
  let gen_list range =
    let count = Random.int_incl 2 5 in
    let values =
      List.init count ~f:(fun _ -> random_in_range range)
      |> List.dedup_and_sort ~compare:Int.compare
    in
    String.concat ~sep:"," (List.map values ~f:string_of_int)

  (* All field generators with weights *)
  let field_generators =
    [
      (25, gen_single_value);
      (25, gen_wildcard);
      (25, gen_range);
      (* (15, gen_step); *)
      (25, gen_list);
    ]

  (* Select a generator based on weights *)
  let select_generator () =
    let total = List.sum (module Int) field_generators ~f:fst in
    let r = Random.int total in
    let rec find acc = function
      | [] -> snd (List.hd_exn field_generators)
      | (weight, gen) :: rest ->
          if r < acc + weight then gen else find (acc + weight) rest
    in
    find 0 field_generators

  (* Generate a single field *)
  let gen_field range =
    let generator = select_generator () in
    generator range

  (* Generate a complete cron expression *)
  let gen_cron_expr () =
    let fields =
      [ minute; hour; day_of_month; month; day_of_week ]
      |> List.map ~f:gen_field
    in
    String.concat ~sep:" " fields

  (* Generate N unique cron expressions *)
  let gen_test_cases n =
    let rec loop acc remaining =
      if remaining <= 0 then acc
      else
        let expr = gen_cron_expr () in
        if List.mem acc expr ~equal:String.equal then loop acc remaining
        else loop (expr :: acc) (remaining - 1)
    in
    loop [] n
end

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

(* Format times showing both UTC and local timezone *)
let format_times_debug times =
  List.map times ~f:(fun t ->
      sprintf "%s (UTC: %s)"
        (Time.to_string_abs ~zone t)
        (Time.to_string_abs ~zone:Time.Zone.utc t))
  |> String.concat ~sep:"\n    "

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
         (format_times_debug expected)
         (format_times_debug actual))

(* Test a single cron expression *)
let test_cron_expr cron_expr ~start_from ~count =
  let actual = upcoming ~start_from ~count (parse cron_expr) in
  let expected = get_croniter_results cron_expr start_from count in
  compare_results cron_expr start_from expected actual

(* Test cases: combination of hand-written and generated *)
let handwritten_test_cases =
  [
    "30 14 * * *";
    (* Daily at 2:30 PM *)
    "0 0 * * 1";
    (* Weekly on Monday at midnight *)
    "0 0 1 * *";
    (* First day of month *)
    "0 9-17 * * 1-5";
    (* Business hours on weekdays *)
    "0,30 * * * *";
    (* On the hour and half hour *)
    "0 0 * * 0" (* Weekly on Sunday *);
  ]

(* Generate fuzz test cases *)
let generated_test_cases =
  let seed = Random.State.make [| 42 |] in
  Random.set_state seed;
  CronGen.gen_test_cases 100

let test_cases = handwritten_test_cases @ generated_test_cases

let%test_unit "compare against croniter reference implementation" =
  if check_main_py_available () then
    let start_from = Time.now () in
    let count = 5 in

    List.iteri test_cases ~f:(fun i cron_expr ->
        printfn "Test #%d: %s" (i + 1) cron_expr;
        try
          match test_cron_expr cron_expr ~start_from ~count with
          | Ok () -> ()
          | Error msg ->
              eprintf "\n%s\n" msg;
              failwith "Test failed"
        with exn ->
          let backtrace = Printexc.get_backtrace () in
          eprintf "%s\n" (Exn.to_string exn);
          eprintf "Backtrace:\n%s\n" backtrace;
          failwith "Test failed")

(* let%test_unit "handwritten" =
  let start_from = Time.now () in
  let count = 5 in

  let handwritten_test_cases = [ "7 6,7 31 11 0" ] in

  List.iter handwritten_test_cases ~f:(fun cron_expr ->
      let _ = printfn "Test: %s" cron_expr in
      try
        match test_cron_expr cron_expr ~start_from ~count with
        | Ok () -> ()
        | Error msg ->
            eprintf "\n%s\n" msg;
            failwith "Test failed"
      with exn ->
        let backtrace = Printexc.get_backtrace () in
        eprintf "%s\n" (Exn.to_string exn);
        eprintf "Backtrace:\n%s\n" backtrace;
        failwith "Test failed") *)
