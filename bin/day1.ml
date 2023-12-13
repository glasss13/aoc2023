let file = "day1.txt"
let int_value c = int_of_char c - int_of_char '0'

let rec calc_line_num line =
  match (line.[0], line.[String.length line - 1]) with
  | '0' .. '9', '0' .. '9' ->
      (int_value line.[0] * 10) + int_value line.[String.length line - 1]
  | '0' .. '9', _ -> calc_line_num (String.sub line 0 (String.length line - 1))
  | _, '0' .. '9' -> calc_line_num (String.sub line 1 (String.length line - 1))
  | _ -> calc_line_num (String.sub line 1 (String.length line - 2))

let read_lines file_name =
  let in_channel = open_in file_name in
  let rec acc_lines acc =
    try
      let line = input_line in_channel in
      acc_lines (line :: acc)
    with End_of_file -> acc
  in
  Fun.protect
    ~finally:(fun () -> close_in_noerr in_channel)
    (fun () -> acc_lines [])

let () =
  let vals = List.map calc_line_num (read_lines file) in
  let result = List.fold_left (fun acc v -> acc + v) 0 vals in
  Printf.printf "answer is: %i\n" result
