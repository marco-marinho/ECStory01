open! Base
open! Stdio
open Angstrom

type triplet =
  { a : int
  ; b : int
  ; c : int
  ; x : int
  ; y : int
  ; z : int
  ; m : int
  }
[@@deriving show]

type eni_result =
  { elements : int array
  ; cycle_start_index : int
  }
[@@deriving show]

let kv label =
  string label
  *> char '='
  *> (take_while1 (function
        | '0' .. '9' | '-' -> true
        | _ -> false)
      >>| Int.of_string)
  <* skip_while (fun c -> Char.equal c ' ')
;;

let line_parser =
  let+ a = kv "A"
  and+ b = kv "B"
  and+ c = kv "C"
  and+ x = kv "X"
  and+ y = kv "Y"
  and+ z = kv "Z"
  and+ m = kv "M" in
  { a; b; c; x; y; z; m }
;;

let parse_line line =
  match parse_string ~consume:All line_parser line with
  | Ok result -> result
  | Error msg -> failwith msg
;;

let eni n imod =
  let seen = Hashtbl.create (module Int) in
  let rec aux i prev_score acc =
    let new_score = prev_score * n % imod in
    match Hashtbl.find seen new_score with
    | Some idx -> { elements = Array.of_list_rev acc; cycle_start_index = idx }
    | None ->
      let () = Hashtbl.set seen ~key:new_score ~data:i in
      aux (i + 1) new_score (new_score :: acc)
  in
  aux 0 1 []
;;

let full_eni exp { elements; cycle_start_index } =
  Sequence.range 0 exp
  |> Sequence.map ~f:(fun i ->
    if i < Array.length elements
    then elements.(i)
    else (
      let wrapped_idx =
        Util.wrap_index i (Array.length elements - cycle_start_index) + cycle_start_index
      in
      elements.(wrapped_idx)))
  |> Sequence.to_list
  |> List.rev
  |> Util.digits_to_int
;;

let last_5_eni exp { elements; cycle_start_index } =
  let last_idx =
    (exp - cycle_start_index) % (Array.length elements - cycle_start_index)
  in
  List.range (last_idx - 5) last_idx
  |> List.map ~f:(fun i ->
    let wrapped_idx =
      Util.wrap_index i (Array.length elements - cycle_start_index) + cycle_start_index
    in
    elements.(wrapped_idx))
  |> List.rev
  |> Util.digits_to_int
;;

let eni_sum exp { elements; cycle_start_index } =
  let pre_cycle =
    Sequence.range 0 cycle_start_index
    |> Sequence.fold ~init:0 ~f:(fun acc i -> acc + elements.(i))
  in
  let post_cycle =
    Sequence.range cycle_start_index (Array.length elements)
    |> Sequence.fold ~init:0 ~f:(fun acc i -> acc + elements.(i))
  in
  let post_cycle_times =
    (exp - cycle_start_index) / (Array.length elements - cycle_start_index)
  in
  let to_sum = (exp - cycle_start_index) % (Array.length elements - cycle_start_index) in
  let left_over =
    Sequence.range cycle_start_index (cycle_start_index + to_sum)
    |> Sequence.fold ~init:0 ~f:(fun acc i -> acc + elements.(i))
  in
  pre_cycle + (post_cycle * post_cycle_times) + left_over
;;

let calc_enis triplet =
  let enia = full_eni triplet.x (eni triplet.a triplet.m) in
  let enib = full_eni triplet.y (eni triplet.b triplet.m) in
  let enic = full_eni triplet.z (eni triplet.c triplet.m) in
  enia + enib + enic
;;

let calc_enis_last_5 triplet =
  let enia = last_5_eni triplet.x (eni triplet.a triplet.m) in
  let enib = last_5_eni triplet.y (eni triplet.b triplet.m) in
  let enic = last_5_eni triplet.z (eni triplet.c triplet.m) in
  enia + enib + enic
;;

let calc_enis_sum triplet =
  let enia = eni_sum triplet.x (eni triplet.a triplet.m) in
  let enib = eni_sum triplet.y (eni triplet.b triplet.m) in
  let enic = eni_sum triplet.z (eni triplet.c triplet.m) in
  enia + enib + enic
;;

let part1 () =
  let input = Util.read_input_lines 1 1 in
  let triplets = List.map ~f:parse_line input in
  let enis = List.map ~f:calc_enis triplets in
  let result = Util.list_max enis in
  result |> Int.to_string
;;

let part2 () =
  let input = Util.read_input_lines 1 2 in
  let triplets = List.map ~f:parse_line input in
  let enis = List.map ~f:calc_enis_last_5 triplets in
  let result = Util.list_max enis in
  result |> Int.to_string
;;

let part3 () =
  let input = Util.read_input_lines 1 3 in
  let triplets = List.map ~f:parse_line input in
  let enis = List.map ~f:calc_enis_sum triplets in
  let result = Util.list_max enis in
  result |> Int.to_string
;;

let solve part =
  match part with
  | 1 -> part1 ()
  | 2 -> part2 ()
  | 3 -> part3 ()
  | _ -> failwith "Invalid part number"
;;
