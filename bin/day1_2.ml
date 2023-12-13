let number_word_to_digit =
  [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
  ]

let int_value c = int_of_char c - int_of_char '0'

let word_to_num s =
  let rec find_word words_list =
    match words_list with
    | [] -> None
    | (word, number) :: rest ->
        if String.starts_with ~prefix:word s then Some number
        else find_word rest
  in
  find_word number_word_to_digit

let num_at s =
  match s.[0] with '0' .. '9' -> Some (int_value s.[0]) | _ -> word_to_num s

let rec calc_line_num line off_l off_r =
  let beginning = num_at (String.sub line off_l (String.length line - off_l)) in
  let ending = num_at (String.sub line off_r (String.length line - off_r)) in
  match (beginning, ending) with
  | Some a, Some b -> (a * 10) + b
  | Some _, None -> calc_line_num line off_l (off_r - 1)
  | None, Some _ -> calc_line_num line (off_l + 1) off_r
  | None, None -> calc_line_num line (off_l + 1) (off_r - 1)

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
  let lines = read_lines "day1.txt" in
  let vals =
    List.map (fun line -> calc_line_num line 0 (String.length line - 1)) lines
  in
  let ret = List.fold_left ( + ) 0 vals in
  Printf.printf "The sum of the calibration values is: %i\n" ret
