open! Base
open! Stdio

let zero_pad width str =
  if String.length str >= width
  then str
  else String.make (width - String.length str) '0' ^ str
;;

let read_input_lines quest part =
  In_channel.read_lines
    ("data/quest" ^ zero_pad 2 (Int.to_string quest) ^ "_" ^ Int.to_string part ^ ".txt")
;;

let num_digits n =
  let rec aux count x = if x = 0 then count else aux (count + 1) (x / 10) in
  if n = 0 then 1 else aux 0 n
;;

let digits_to_int lst =
  List.fold_left lst ~init:0 ~f:(fun acc d -> (acc * (10 ** num_digits d)) + d)
;;

let list_max lst = List.fold_left ~init:Int.min_value ~f:max lst

let print_array ~f:converter arr =
  let ostring = Array.fold ~init:"[" ~f:(fun acc x -> acc ^ converter x ^ ",") arr in
  Stdio.print_endline (ostring ^ "]")
;;

let wrap_index idx len = ((idx % len) + len) % len
